library IEEE;
use IEEE.std_logic_1164.all;

package mem is

  component memTest
    generic(
      PIPE_EN    :     boolean := false;  -- enable pipelined operations
      DATA_WIDTH :     natural := 16;   -- memory data width
      ADDR_WIDTH :     natural := 23;   -- memory address width
      BEG_TEST   :     natural := 16#00_0000#;  -- beginning test range address
      END_TEST   :     natural := 16#7F_FFFF#  -- ending test range address
      );
    port(
      clk        : in  std_logic;       -- master clock input
      rst        : in  std_logic;       -- reset
      doAgain    : in  std_logic;       -- re-do memory test
      begun      : in  std_logic;       -- memory operation begun indicator
      done       : in  std_logic;       -- memory operation done indicator
      dIn        : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from memory
      rdPending  : in  std_logic;       -- read operations in progress indicator                                         
      rd         : out std_logic;       -- memory read control signal
      wr         : out std_logic;       -- memory write control signal
      addr       : out std_logic_vector(ADDR_WIDTH-1 downto 0);  -- address to memory
      dOut       : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to memory
      progress   : out std_logic_vector(1 downto 0);  -- memory test progress indicator
      err        : out std_logic        -- memory error flag
      );
  end component;

end package mem;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use WORK.common.all;
use WORK.rand.all;

entity memTest is
  generic(
    PIPE_EN    :     boolean := false;  -- enable pipelined operations
    DATA_WIDTH :     natural := 16;     -- memory data width
    ADDR_WIDTH :     natural := 23;     -- memory address width
    BEG_TEST   :     natural := 16#00_0000#;  -- beginning test range address
    END_TEST   :     natural := 16#7F_FFFF#  -- ending test range address
    );
  port(
    clk        : in  std_logic;         -- master clock input
    rst        : in  std_logic;         -- reset
    doAgain    : in  std_logic;         -- re-do memory test
    begun      : in  std_logic;         -- memory operation begun indicator
    done       : in  std_logic;         -- memory operation done indicator
    dIn        : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from memory
    rdPending  : in  std_logic;         -- read operations in progress indicator                                         
    rd         : out std_logic;         -- memory read control signal
    wr         : out std_logic;         -- memory write control signal
    addr       : out std_logic_vector(ADDR_WIDTH-1 downto 0);  -- address to memory
    dOut       : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to memory
    progress   : out std_logic_vector(1 downto 0);  -- memory test progress indicator
    err        : out std_logic          -- memory error flag
    );
end memTest;


architecture arch of memTest is

  -- states of the memory tester state machine
  type testState is (
    INIT,                               -- init
    LOAD,                               -- load memory with pseudo-random data
    COMPARE,                            -- compare memory contents with pseudo-random data
    EMPTY_PIPE,                         -- empty read pipeline
    STOP                                -- stop and indicate memory status
    );
  signal state_r, state_x : testState;  -- state register and next state

  -- registers
  signal addr_r, addr_x : unsigned(addr'range);  -- address register
  signal err_r, err_x   : std_logic;             -- error flag

  -- internal signals
  signal ld   : std_logic;              -- load random number gen with seed value
  signal cke  : std_logic;              -- clock-enable for random number gen
  signal rand : std_logic_vector(dOut'range);   -- random number from generator
  signal seed : std_logic_vector(dIn'range);    -- random number starting seed

begin

  seed <= (others => '1');              -- random number seed is 111...111

  -- random number generator module
  u0 : randGen
    generic map(
      DATA_WIDTH => seed'length
      )
    port map(
      clk        => clk,                -- input clock
      cke        => cke,                -- clock-enable to control when new random num is computed
      ld         => ld,                 -- load seed control signal
      seed       => seed,               -- random number seed
      rand       => rand                -- random number output from generator
      );

  -- connect internal registers to external busses      
  addr <= std_logic_vector(addr_r);     -- memory address bus driven by memory address register
  dOut <= rand;                         -- always output the current random number to the memory
  err  <= err_r;                        -- output the current memory error status

  -- memory test controller state machine operations
  combinatorial : process(state_r, err_r, addr_r, dIn, rand, begun, done, rdPending, doAgain)
  begin

    -- default operations (do nothing unless explicitly stated in the following case statement)
    rd      <= NO;                      -- no memory write
    wr      <= NO;                      -- no memory read
    ld      <= NO;                      -- don't load the random number generator
    cke     <= NO;                      -- don't generate a new random number
    addr_x  <= addr_r;                  -- next address is the same as current address
    err_x   <= err_r;                   -- error flag is unchanged
    state_x <= state_r;                 -- no change in memory tester state

    -- **** compute the next state and operations ****
    case state_r is

      ------------------------------------------------------
      -- initialize the memory test controller
      ------------------------------------------------------
      when INIT =>
        ld       <= YES;                -- load random number seed
        cke      <= YES;                -- enable clocking of rand num gen so seed gets loaded
        addr_x   <= TO_UNSIGNED(BEG_TEST,addr_x'length);           -- load starting mem address
        err_x    <= NO;                 -- clear memory error flag
        state_x  <= LOAD;               -- next go to LOAD state and write pattern to memory
        progress <= "00";               -- indicate the current controller state

      when LOAD =>                      -- load the memory with data from the random number generator
        progress      <= "01";          -- indicate the current controller state
        if PIPE_EN then
          wr          <= YES;
          if begun = YES then
            if addr_r /= END_TEST then
              addr_x  <= addr_r + 1;    -- so increment address
              cke     <= YES;           -- and enable generator clock to get new random num
            else
              cke     <= YES;           -- enable generator clock and
              ld      <= YES;           -- reload the generator with the seed value
              addr_x  <= TO_UNSIGNED(BEG_TEST, addr_x'length);  -- reset to start of test range
              state_x <= COMPARE;
            end if;
          end if;
        else
          if done = NO then
            wr        <= YES;
          elsif addr_r /= END_TEST then
            addr_x    <= addr_r + 1;    -- so increment address
            cke       <= YES;           -- and enable generator clock to get new random num
          else
            cke       <= YES;           -- enable generator clock and
            ld        <= YES;           -- reload the generator with the seed value
            addr_x    <= TO_UNSIGNED(BEG_TEST, addr_x'length);  -- reset to start of test range
            state_x   <= COMPARE;
          end if;
        end if;

      when COMPARE =>                   -- re-run the generator and compare it to memory contents
        progress      <= "10";          -- indicate the current controller state
        if PIPE_EN then
          rd          <= YES;
          if begun = YES then
            addr_x    <= addr_r + 1;    -- increment address to check next memory location
          end if;
          if addr_r = END_TEST then
            state_x   <= EMPTY_PIPE;
          end if;
          if done = YES then
            if dIn /= rand then         -- compare value from memory to random number
              err_x   <= YES;           -- error if they don't match
            end if;
            cke       <= YES;           -- enable generator clock to get next random num
          end if;
        else
          if done = NO then             -- current read operation is not complete
            rd        <= YES;           -- keep read signal active since memory read is not done
          else                          -- current read operation is complete
            rd        <= NO;            -- release the read signal when read op is complete
            if dIn /= rand then         -- compare value from memory to random number
              err_x   <= YES;           -- error if they don't match
            end if;
            if addr_r = END_TEST then
              state_x <= STOP;          -- go to STOP state once entire range has been checked
            end if;
            addr_x    <= addr_r + 1;    -- increment address to check next memory location
            cke       <= YES;           -- and enable generator clock to get next random num
          end if;
        end if;

      when EMPTY_PIPE =>
        progress  <= "10";              -- indicate the current controller state
        if done = YES then
          if dIn /= rand then           -- compare value from memory to random number
            err_x <= YES;               -- error if they don't match
          end if;
          cke     <= YES;               -- enable generator clock to get next random num
        end if;
        if rdPending = NO then
          state_x <= STOP;
        end if;

      when others =>                    -- memory test is complete
        progress  <= "11";              -- indicate the current controller state
        if doAgain = YES then
          ld      <= YES;               -- load random number seed
          cke     <= YES;               -- enable clocking of rand num gen so seed gets loaded
          addr_x  <= TO_UNSIGNED(BEG_TEST, addr_x'length);  -- load starting mem address
          err_x   <= NO;                -- clear memory error flag
          state_x <= INIT;              -- go to the INIT state and and re-do memory test
        end if;

    end case;

  end process;


  -- update the registers of the memory tester controller       
  update : process(clk)
  begin
    if clk'event and clk = '1' then
      if rst = YES then
        -- go to starting state and clear error flag when reset occurs
        state_r <= INIT;
      else
        -- update error flag, address register, and state
        err_r   <= err_x;
        addr_r  <= addr_x;
        state_r <= state_x;
      end if;
    end if;
  end process;

end arch;
