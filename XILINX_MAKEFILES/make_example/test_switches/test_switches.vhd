library ieee;
use ieee.std_logic_1164.all;


entity test_switches is
  port(
    sw1 : in  std_logic_vector(4 downto 1);  -- DIP switches
    sw2 : in  std_logic;                     -- pushbutton
    -- sw3 : in  std_logic;                  -- pushbutton
    s   : out std_logic_vector(6 downto 0)   -- XS Board LED digit
    );
end entity;


architecture arch of test_switches is
  signal sw3 : std_logic;
begin

  sw3 <= sw2;

  -- light the XS Board LED digit with the pattern from the
  -- DIP switches if both pushbuttons are pressed.
  -- these LED segments are active-high.
  s(3 downto 0) <= sw1(4 downto 1)     when (sw2 = '1' and sw3 = '1') else
                   not sw1(4 downto 1) when (sw2 = '0' and sw3 = '0') else
                   "0000";              -- otherwise keep LED digit dark
  s(5)          <= sw2;
  s(4)          <= sw3;
  s(6)          <= '1';

end architecture;
