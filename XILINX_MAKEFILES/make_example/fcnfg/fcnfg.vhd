--
-- CPLD design which controls the configuration of the XSA FPGA
-- with data from the Flash chip.
--


library ieee, unisim;
use unisim.vcomponents.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;


entity fcnfg is
  generic(
    ADDR_LEN   :     positive := 18     -- number of Flash address bits
    );
  port(
    master_clk : in  std_logic;         -- clock from oscillator
    clka       : out std_logic;         -- FPGA global clock input

    -- Flash address and control pins
    flash_a       : out std_logic_vector(ADDR_LEN-1 downto 0);  -- Flash address
    flash_ce_n    : out std_logic;      -- Flash chip-enable
    flash_oe_n    : out std_logic;      -- Flash output-enable
    flash_we_n    : out std_logic;      -- Flash write-enable
    flash_reset_n : out std_logic;      -- Flash reset

    -- FPGA configuration pins
    fpga_prog_n   : out std_logic;      -- FPGA PROGRAM pin
    fpga_cclk     : out std_logic;      -- FPGA config clock
    fpga_m        : out std_logic_vector(0 downto 0);  -- FPGA configuration mode pin
    fpga_cs_n     : out std_logic;      -- FPGA config chip-select
    fpga_wr_n     : out std_logic;      -- FPGA config write-enable
    fpga_init_n   : in  std_logic;      -- FPGA config init status
    fpga_dout_bsy : in  std_logic;      -- FPGA config busy status
    fpga_done     : in  std_logic       -- FPGA config done status
    );
end entity;


architecture arch of fcnfg is

  constant NO                  : std_logic                    := '0';
  constant YES                 : std_logic                    := '1';
  constant LO                  : std_logic                    := '0';
  constant HI                  : std_logic                    := '1';
  constant FLOAT               : std_logic                    := 'Z';
  constant SLAVE_PARALLEL_MODE : std_logic_vector(0 downto 0) := "0";

  signal clk_div       : std_logic_vector(6 downto 0);
  signal cclk          : std_logic;
  signal program_n     : std_logic;
  signal cs            : std_logic;
  signal addr          : std_logic_vector(ADDR_LEN-1 downto 0);
  signal poweron_reset : std_logic;
  signal poweron_cnt   : std_logic_vector(19 downto 0);

  attribute init                : string;
  attribute init of poweron_cnt : signal is "11111111111111111111";

begin

  -- place FPGA in config. mode to receive parallel data from the Flash
  fpga_m <= SLAVE_PARALLEL_MODE;

  -- Flash is enabled for reading while FPGA is not yet configured
  -- and then the Flash pins float when configuration is done
  flash_oe_n    <= LO when fpga_done = NO else FLOAT;
  flash_ce_n    <= LO when fpga_done = NO else FLOAT;
  flash_we_n    <= HI when fpga_done = NO else FLOAT;  -- disable Flash writes
  flash_reset_n <= HI;

  -- generate configuration clock for FPGA from the XSA clock.
  -- The XSA clock could be as much as 100 MHz, so divide by 16
  -- so as not to violate the access time of the Flash.
  process(master_clk)
  begin
    if rising_edge(master_clk) then
      clk_div <= clk_div + 1;
    end if;
  end process;
  cclk        <= clk_div(3);            -- internal configuration clock
  fpga_cclk   <= cclk;                  -- also send config. clock to FPGA

  -- pass user clocks to the FPGA after it is configured
  clka <= master_clk when fpga_done = YES else FLOAT;
  -- clkb <= clk_div(0) when fpga_done = YES else FLOAT;

  -- Apply reset when the power to the XSA Board is first applied.
  -- Remove the power-on reset after the counter reaches 0.
  process(cclk)
  begin
    if rising_edge(cclk) then
      if poweron_cnt = 0 then
        poweron_reset <= NO;            -- remove reset when timeout expires
        else
          poweron_cnt <= poweron_cnt - 1;
        poweron_reset <= YES;
      end if;
    end if;
  end process;

  -- initiate FPGA configuration by lowering the PROGRAM# pin
  -- during the initial power-on reset and then raising it when
  -- the power-on timeout expires and the manual program control is high
  program_n   <= LO when poweron_reset = YES else HI;
  fpga_prog_n <= LO when program_n = LO      else FLOAT;

  -- Select the FPGA for configuration as long as the /PROGRAM pin
  -- is not held low and the INIT pin is not low.
  process(cclk, program_n)
  begin
    if(program_n = LO) then
      cs <= LO;
    elsif rising_edge(cclk) then
      cs <= fpga_init_n;
    end if;
  end process;

  -- Select the FPGA for configuration by lowering its chip-select
  -- and write inputs when the internal chip-select is high.  Then
  -- float these pins after the FPGA configuration is done.
  fpga_cs_n <= not cs when fpga_done = NO else FLOAT;
  fpga_wr_n <= not cs when fpga_done = NO else FLOAT;

  -- increment the Flash address so the next byte of configuration
  -- data is presented to the FPGA.  Stop incrementing if the
  -- FPGA is not selected, signals a config. error (INIT=0), or
  -- is busy.  Reset the address counter to zero whenever the
  -- /PROGRAM pin goes low and a new configuration sequence begins.
  process(cclk)
  begin
    if rising_edge(cclk) then
      if (cs = HI) and (fpga_init_n = HI) and (fpga_dout_bsy = LO) then
        addr <= addr + 1;
      elsif program_n = LO then
        addr <= (others => LO);
      end if;
    end if;
  end process;

  -- pass the Flash address out to the Flash chip.  Float the address
  -- lines once configuration is done.
  flash_a <= addr when fpga_done = NO else (others => FLOAT);

end architecture;
