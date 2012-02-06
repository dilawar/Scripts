----------------------------------------------------------------------------------------
-- A simple interface to the stereo codec on the XST and XSB-300E Boards
----------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use WORK.common.all;


package audio is

  component codec_intfc
    generic(
      XST_1_3_X  :    boolean := false;  -- select the type of board containing the codec
      XST_2_0_X  :    boolean := false;
      XST_2_1_X  :    boolean := false;
      XSB_300E   :    boolean := false;
      DIVISOR    :    natural := 4;     -- ratio of clk freq / mclk freq
      RESOLUTION :    natural := 20     -- bits of resolution in codec ADC and DAC converters
      );
    port(
      clk        : in std_logic;        -- main clock
      rst_n      : in std_logic;        -- reset

      -- host side
      left_chan : out std_logic;        -- high when left channel is active; low when right channel is active
      ADC       : out std_logic_vector(RESOLUTION-1 downto 0);  -- digitized output from analog->digital converters
      ADC_rdy   : out std_logic;        -- high when output from ADC is ready
      DAC       : in  std_logic_vector(RESOLUTION-1 downto 0);  -- input to codec digital->analog converters 
      DAC_rdy   : out std_logic;        -- high when input to DAC is needed

      -- codec side
      mclk : out std_logic;             -- master clock
      sclk : out std_logic;             -- serial bit clock = mclk / 4
      lrck : out std_logic;             -- left-right clock = sclk / 64
      sdti : out std_logic;             -- serial data to left and right DACs
      sdto : in  std_logic              -- serial data from left and right ADCs
      );
  end component;

end package audio;



library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use WORK.common.all;


entity codec_intfc is
  generic(
    XST_1_3_X  :    boolean := false;   -- select the type of board containing the codec
    XST_2_0_X  :    boolean := false;
    XST_2_1_X  :    boolean := false;
    XSB_300E   :    boolean := false;
    DIVISOR    :    natural := 4;       -- ratio of clk freq / mclk freq
    RESOLUTION :    natural := 20       -- bits of resolution in codec ADC and DAC converters
    );
  port(
    clk        : in std_logic;          -- main clock
    rst_n      : in std_logic;          -- reset

    -- host side
    left_chan : out std_logic;          -- high when left channel is active; low when right channel is active
    ADC       : out std_logic_vector(RESOLUTION-1 downto 0);  -- digitized output from analog->digital converters
    ADC_rdy   : out std_logic;          -- high when output from ADC is ready
    DAC       : in  std_logic_vector(RESOLUTION-1 downto 0);  -- input to codec digital->analog converters 
    DAC_rdy   : out std_logic;          -- high when input to DAC is needed

    -- codec side
    mclk : out std_logic;               -- master clock
    sclk : out std_logic;               -- serial bit clock = mclk / 4
    lrck : out std_logic;               -- left-right clock = sclk / 64
    sdti : out std_logic;               -- serial data to left and right DACs
    sdto : in  std_logic                -- serial data from left and right ADCs
    );
end codec_intfc;


architecture arch of codec_intfc is
  signal   ADC_r, ADC_x      : std_logic_vector(RESOLUTION-1 downto 0);  -- shift-reg for receiving bits from the ADCs
  signal   DAC_r, DAC_x      : std_logic_vector(RESOLUTION-1 downto 0);  -- shift-reg for sending bits to the DACs
  signal   DAC_rdy_i         : std_logic;  -- internal version of DAC_rdy
  constant MCLK_SCLK_RATIO   : natural := 4;  -- four mclks per sclk
  constant CHAN_FRAME_LEN    : natural := 32;  -- number of sclks in each left/right serial channel frame
  constant NUM_CHANNELS      : natural := 2;  -- number of codec channels (left & right)
  constant MCLK_BIT          : natural := log2(DIVISOR)-1;  -- position of mclk output in counter
  constant PHASE_CNT_LEN     : natural := log2(DIVISOR * MCLK_SCLK_RATIO);  -- num bits in phase portion of counter
  constant BIT_CNT_LEN       : natural := log2(CHAN_FRAME_LEN);  -- number of bits in bit counter portion of counter
  constant CHAN_CNT_LEN      : natural := log2(NUM_CHANNELS);  -- number of bits in left/right channel portion of counter
  constant CNT_LEN           : natural := PHASE_CNT_LEN + BIT_CNT_LEN + CHAN_CNT_LEN;  -- counter = left/right & bit counter & phase
  constant SCLK_RISING_EDGE  : natural := (2**(PHASE_CNT_LEN-1))-1;  -- value of phase when sclk has a rising edge
  constant SCLK_FALLING_EDGE : natural := (2**PHASE_CNT_LEN)-1;  -- value of phase when sclk has a falling edge
  constant left              : natural := 1;  -- value of lrck when left channel is accesssed
  constant right             : natural := 0;  -- value of lrck when right channel is accesssed
  signal   cnt_r, cnt_x      : unsigned(CNT_LEN-1 downto 0);  -- counter for generating all the codec timing signals
  -- alias for the LSBits of the counter that indicate the phase within each serial bit interval
  alias phase                : unsigned(PHASE_CNT_LEN-1 downto 0) is cnt_r(PHASE_CNT_LEN-1 downto 0);
  -- alias for the upper bits of the counter that indicate the bit slot within each left/right channel frame
  alias bit_cnt              : unsigned( BIT_CNT_LEN-1 downto 0) is cnt_r(BIT_CNT_LEN + PHASE_CNT_LEN - 1 downto PHASE_CNT_LEN);
  signal   DAC_rdy_cnt       : natural;  -- load DAC with parallel data when bit counter hits this value
  signal   DAC_start_cnt     : natural;  -- start shifting data from DAC when bit counter hits this value
  signal   left_chan_cnt     : unsigned(CNT_LEN-1 downto PHASE_CNT_LEN);  -- counter for generating host-side left_chan signal
begin

  -- increment the counter and generate the codec clocks from the appropriate counter bits
  -- (place inverters on these clocks when using the XST-1.3.X Board)
  cnt_x <= cnt_r + 1;
  mclk  <= cnt_r(MCLK_BIT)        when (not XST_1_3_X) else not cnt_r(MCLK_BIT);
  sclk  <= cnt_r(PHASE_CNT_LEN-1) when (not XST_1_3_X) else not cnt_r(PHASE_CNT_LEN-1);
  lrck  <= cnt_r(CNT_LEN-1)       when (not XST_1_3_X) else not cnt_r(CNT_LEN-1);

  -- tell the host side which channel it is accessing.  This signal is not just a copy of the LRCK signal
  -- because for the XST-1.3.X and XST-2.0.X Boards we have to load the DAC register for the right channel
  -- on the last SCLK cycle of the preceding left LRCK phase (and similarly when loading the DAC register
  -- for the left channel).  So we have to advance the host-side left/right indicator a few SCLK cycles to
  -- correct for this.  But not more than 11 cycles or we will put out an erroneous left/right indicator
  -- when the ADC register is ready to be read.  Eight cycles is good because it minimizes the amount of
  -- adding circuitry that is created.
  left_chan_cnt <= cnt_r(CNT_LEN-1 downto PHASE_CNT_LEN) + 8;
  left_chan     <= left_chan_cnt(CNT_LEN-1);  -- indicates the currently active codec channel

  -- gather the bits from the codec ADC into the ADC shift register
  -- SDTO changes on the falling edge of SCLK, so we gather the bits on the rising edge when they are stable
  ADC_x   <= (ADC_r(RESOLUTION-2 downto 0) & sdto) when (phase = SCLK_RISING_EDGE) and (bit_cnt < RESOLUTION) else ADC_r;
  -- tell the host when all the bits from the codec ADC have been received
  ADC_rdy <= YES                                   when (phase = SCLK_RISING_EDGE) and (bit_cnt = RESOLUTION) else NO;
  -- connect the parallel output from the ADC shift register to the host
  ADC     <= ADC_r;

  -- the output from the DAC starts in bit slot 12 of the frame for the XST-2.1.X but it starts
  -- at the beginning of the frame (bit slot 0) for the XST-1.3.X, XST-2.0.X and XSB-300E Boards
  DAC_start_cnt <= 0                                  when not XST_2_1_X                                              else 12;
  -- data is needed one SCLK cycle before the DAC shift register starts shifting
  DAC_rdy_cnt   <= CHAN_FRAME_LEN-1                   when not XST_2_1_X                                              else 11;
  -- tell the host when the DAC shift register needs to be loaded with a value
  DAC_rdy_i     <= YES                                when (phase = SCLK_FALLING_EDGE) and (bit_cnt = DAC_rdy_cnt)    else NO;
  DAC_rdy       <= DAC_rdy_i;
  -- load the DAC shift register and shift its contents over to the codec                                               
  DAC_x         <= DAC                                when DAC_rdy_i = YES                                            else  -- parallel load the DAC shift register with a new value from the host
                                        -- else shift the bits out of the DAC shift register and into the codec DAC.
                                        -- The codec DAC accepts serial bits on the rising edge of SCLK, so we output on the falling edge
                                        -- to make sure the bits are stable
                   DAC_r(RESOLUTION-2 downto 0) & '0' when (phase = SCLK_FALLING_EDGE) and (bit_cnt >= DAC_start_cnt) else
                   DAC_r;
  -- connect the MSBit of the DAC shift register to the codec SDTI pin
  -- (place inverter on SDTI when using the XST-1.3.X Board)
  sdti          <= DAC_r(RESOLUTION-1)                when not XST_1_3_X                                              else not DAC_r(RESOLUTION-1);

  process(rst_n, clk)
  begin
    -- active-low reset asynchronously clears the counter and shift-registers
    if(rst_n = LO) then
      cnt_r <= (others => '0');
      ADC_r <= (others => '0');
      DAC_r <= (others => '0');
      -- update the counter and shift registers on the rising clock edge
    elsif(clk'event and clk = HI) then
      cnt_r <= cnt_x;
      ADC_r <= ADC_x;
      DAC_r <= DAC_x;
    end if;
  end process;

end arch;




----------------------------------------------------------------------------------------
-- A simple loopback test circuit follows
----------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use WORK.audio.all;
use WORK.common.all;

entity test_codec is
  port(
    clka         : in  std_logic;       -- 50 MHz clock from programmable oscillator
    rst_n        : in  std_logic;       -- active-low reset from pushbutton
    mclk         : out std_logic;
    sclk         : out std_logic;
    lrck         : out std_logic;
    sdti         : out std_logic;
    sdto         : in  std_logic;
    ADC_rdy_diag : out std_logic;       -- diagnostic outputs so we can see the 
    DAC_rdy_diag : out std_logic        -- ADC_rdy and DAC_rdy signals
    );
end test_codec;


architecture arch of test_codec is
  signal ADC, left_ADC, right_ADC, DAC : std_logic_vector(19 downto 0);
  signal left_chan, ADC_rdy, DAC_rdy   : std_logic;
begin

  -- instantiate the codec interface
  u0 : codec_intfc
    generic map(
-- XST_1_3_X => TRUE,                   -- set the type of board being used
--                      XST_2_0_X       =>      TRUE,  -- set the type of board being used
      XST_2_1_X => true,                -- set the type of board being used
--                      XSB_300E        =>      TRUE,  -- set the type of board being used
      DIVISOR   => 4                    -- divide 50 MHz clk down to 12.5 MHz MCLK
      )
    port map(
      clk       => clka,
      rst_n     => rst_n,
      left_chan => left_chan,
      ADC       => ADC,
      ADC_rdy   => ADC_rdy,
      DAC       => DAC,
      DAC_rdy   => DAC_rdy,
      mclk      => mclk,
      sclk      => sclk,
      lrck      => lrck,
      sdti      => sdti,
      sdto      => sdto
      );

  -- clock the ADC output from the codec interface into the     left or
  -- right ADC register depending upon which codec channel is active
  process(clka)
  begin
    if(rising_edge(clka)) then
      if ADC_rdy = YES then
        if left_chan = YES then
          left_ADC  <= ADC;
        else
          right_ADC <= ADC;
        end if;
      end if;
    end if;
  end process;

  -- connect the left or right ADC register to the DAC input of the
  -- codec interface depending upon which channel is active.
  -- This loops the codec input back to its output.
  DAC <= left_ADC when left_chan=YES else right_ADC;

  -- connect the diagnostic outputs so we can examine the timing
--	DAC_rdy_diag <= DAC_rdy;
--	ADC_rdy_diag <= ADC_rdy;
  DAC_rdy_diag <= '0';
  ADC_rdy_diag <= '0';

end arch;
