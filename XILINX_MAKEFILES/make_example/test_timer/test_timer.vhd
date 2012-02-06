library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


entity test_timer is
  port (
    rst_n  : in  std_logic;             -- synchronous reset
    clka   : in  std_logic;             -- counter clock
    strobe : out std_logic              -- should flash every 2^25 / 50,000,000 = 2/3 seconds
    );
end entity;


architecture arch of test_timer is
  signal cnt : unsigned(25 downto 0);
begin

  process(clka)
  begin
    if rising_edge(clka) then
      if rst_n = '0' then
        cnt <= TO_UNSIGNED(0, 25);
      else
        cnt <= cnt + 1;
      end if;
    end if;
  end process;

  strobe <= cnt(24);
end architecture;

