library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use WORK.common.all;
use WORK.xsasdram.all;
use WORK.ramintfc_pckg.all;

entity ramintfc is
  generic(
    FREQ        :       natural := 50_000;  -- clock frequency in KHz
    DATA_WIDTH  :       natural := 16;  -- width of data
    HADDR_WIDTH :       natural := 24;  -- host-side address width
    SADDR_WIDTH :       natural := 13;  -- SDRAM address bus width
    NROWS       :       natural := 4096;  -- number of rows in each SDRAM bank
    NCOLS       :       natural := 256  -- number of words in each row
    );
  port(
    -- I/O
    ppd         : in    std_logic_vector(7 downto 0);  -- parallel port data
    pps         : out   std_logic_vector(6 downto 3);  -- parallel port status
    ce_n        : out   std_logic;      -- Flash RAM chip-enable
    clkin       : in    std_logic;      -- main clock input
    clkfb       : in    std_logic;      -- feedback clock mirrors SDRAM clock
    data        : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- data bus to SDRAM
    clkout      : out   std_logic;      -- clock to SDRAM
    cke         : out   std_logic;      -- SDRAM clock-enable
    cs_n        : out   std_logic;      -- SDRAM chip-select
    ras_n       : out   std_logic;      -- SDRAM RAS
    cas_n       : out   std_logic;      -- SDRAM CAS
    we_n        : out   std_logic;      -- SDRAM write-enable
    ba          : out   std_logic_vector( 1 downto 0);  -- SDRAM bank-address
    saddr       : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM address bus
    dqmh        : out   std_logic;      -- SDRAM DQMH
    dqml        : out   std_logic;      -- SDRAM DQML
    s           : out   std_logic_vector(6 downto 0)  -- 7-segment LED
    );
end entity;


architecture arch of ramintfc is
begin

  u1 : ramintfc_core generic map (
    FREQ        => FREQ,
    DATA_WIDTH  => DATA_WIDTH,
    HADDR_WIDTH => HADDR_WIDTH,
    SADDR_WIDTH => SADDR_WIDTH,
    NROWS       => NROWS,
    NCOLS       => NCOLS
    )
    port map (
      ppd       => ppd,
      pps       => pps,
      ce_n      => ce_n,
      clkin     => clkin,
      clkfb     => clkfb,
      data      => data,
      clkout    => clkout,
      cke       => cke,
      cs_n      => cs_n,
      ras_n     => ras_n,
      cas_n     => cas_n,
      we_n      => we_n,
      ba        => ba,
      saddr     => saddr,
      dqmh      => dqmh,
      dqml      => dqml,
      s         => s
      );

end architecture;
