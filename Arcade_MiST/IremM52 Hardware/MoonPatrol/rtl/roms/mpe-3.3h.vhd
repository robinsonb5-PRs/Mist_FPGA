library ieee;
use ieee.std_logic_1164.all,ieee.numeric_std.all;

entity mpe_33h is
port (
	clk  : in  std_logic;
	addr : in  std_logic_vector(11 downto 0);
	data : out std_logic_vector(7 downto 0)
);
end entity;

architecture prom of mpe_33h is
	type rom is array(0 to  4095) of std_logic_vector(7 downto 0);
	signal rom_data: rom := (
		X"00",X"00",X"00",X"11",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"00",X"00",X"01",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"11",X"8F",X"01",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"77",X"EF",X"0D",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"FF",X"FF",X"0F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"02",X"00",X"00",X"00",X"00",
		X"00",X"11",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"FD",X"FB",X"87",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"44",X"02",X"04",X"00",X"00",X"00",
		X"00",X"33",X"FF",X"CF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"44",X"02",X"04",X"00",X"00",X"00",
		X"00",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"11",X"F3",X"F3",X"C3",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"44",X"02",X"04",X"00",X"00",X"00",
		X"11",X"FF",X"FF",X"EF",X"0F",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"33",X"FF",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"22",X"33",X"0C",X"04",X"00",X"00",X"11",
		X"FF",X"FF",X"FF",X"8F",X"0F",X"0F",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"33",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"22",X"FF",X"8F",X"04",X"00",X"00",X"33",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"0C",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"33",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"11",X"FF",X"EF",X"08",X"00",X"00",X"33",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0C",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"32",X"FC",X"FD",X"E9",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"11",X"FF",X"FF",X"08",X"00",X"00",X"33",
		X"F5",X"F5",X"F5",X"FA",X"FA",X"5A",X"0C",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"0C",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"33",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"88",X"11",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"01",X"00",X"33",X"F5",X"FA",X"0C",X"00",X"00",X"11",
		X"F5",X"F5",X"F5",X"FA",X"FA",X"5A",X"08",X"00",X"00",X"00",X"00",X"00",X"01",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"11",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"33",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"88",X"11",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"01",X"00",X"77",X"FF",X"FF",X"0E",X"00",X"00",X"33",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0C",X"00",X"00",X"00",X"00",X"00",X"05",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"FF",X"EF",X"0C",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"11",X"F1",X"F1",X"E1",X"00",X"00",X"00",X"00",X"00",X"44",X"22",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"01",X"00",X"77",X"FF",X"EF",X"0E",X"00",X"00",X"77",
		X"F8",X"FC",X"F3",X"FC",X"F3",X"E1",X"0E",X"00",X"00",X"00",X"00",X"00",X"05",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"11",X"F1",X"F1",X"E1",X"00",X"00",X"00",X"00",X"00",X"44",X"22",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"01",X"00",X"77",X"FF",X"CF",X"0E",X"00",X"00",X"77",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0E",X"00",X"00",X"00",X"00",X"00",X"05",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"32",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"22",X"44",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"11",X"00",X"33",X"FF",X"FF",X"0C",X"00",X"11",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"08",X"00",X"00",X"00",X"00",X"05",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",
		X"00",X"00",X"44",X"11",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"00",X"22",X"44",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"22",X"06",X"00",X"32",X"7D",X"B6",X"84",X"00",X"11",X"F0",
		X"FC",X"F0",X"FC",X"F3",X"F0",X"F3",X"F0",X"08",X"00",X"00",X"00",X"00",X"05",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"00",X"00",
		X"00",X"00",X"55",X"FF",X"FF",X"FF",X"EF",X"08",X"00",X"00",X"00",X"00",X"22",X"44",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"11",X"FF",X"08",X"33",X"FF",X"FF",X"0C",X"00",X"11",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",X"EF",X"08",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"EF",X"0F",X"08",X"00",X"00",X"00",
		X"00",X"00",X"33",X"FF",X"FF",X"FF",X"EF",X"08",X"00",X"00",X"00",X"00",X"11",X"88",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"EF",X"0C",X"33",X"FF",X"FF",X"0C",X"00",X"33",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0C",X"00",X"00",X"00",X"11",X"FF",X"0C",X"00",X"00",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"F5",X"F5",X"FA",X"5A",X"08",X"00",X"00",X"00",
		X"00",X"00",X"77",X"FF",X"FF",X"FF",X"EF",X"08",X"00",X"00",X"00",X"33",X"11",X"88",X"CC",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"F6",X"0E",X"77",X"FF",X"FF",X"8E",X"00",X"77",X"FF",
		X"ED",X"ED",X"FF",X"FF",X"B7",X"B7",X"CF",X"0E",X"00",X"00",X"00",X"33",X"FF",X"8E",X"00",X"00",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"0F",X"08",X"00",X"00",X"00",
		X"00",X"00",X"77",X"FF",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"00",X"77",X"99",X"99",X"EE",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"0E",X"77",X"FF",X"FF",X"8E",X"00",X"77",X"FF",
		X"ED",X"ED",X"FF",X"FF",X"B7",X"B7",X"8F",X"0E",X"00",X"00",X"00",X"77",X"FF",X"8F",X"00",X"00",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FE",X"FD",X"F6",X"F7",X"CF",X"08",X"00",X"00",X"00",X"77",X"99",X"99",X"EE",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"32",X"76",X"04",X"77",X"FF",X"FF",X"8E",X"00",X"33",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"0C",X"00",X"00",X"00",X"74",X"FD",X"EF",X"00",X"00",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"0F",X"08",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"8F",X"08",X"00",X"00",X"00",X"33",X"11",X"88",X"CC",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"0E",X"75",X"FA",X"F5",X"CA",X"00",X"33",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"0C",X"00",X"00",X"00",X"77",X"FF",X"FF",X"00",X"02",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"8F",X"08",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"EF",X"0E",X"00",X"00",X"00",X"11",X"33",X"CC",X"88",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"8F",X"77",X"FF",X"FF",X"CF",X"00",X"00",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"08",X"02",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FC",X"F4",X"F2",X"C3",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"F6",X"FD",X"FF",X"EF",X"0F",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"CF",X"08",
		X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"EF",X"FF",X"FF",X"FF",X"CF",X"08",X"00",X"11",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"08",X"00",X"00",X"00",X"00",X"FA",X"F8",X"2F",X"08",X"02",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"33",X"FB",X"FD",X"FB",X"FD",X"0C",
		X"00",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"EF",X"FF",X"FF",X"FF",X"EF",X"0E",X"00",X"00",
		X"77",X"B7",X"B6",X"6D",X"FC",X"0E",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"08",X"02",
		X"02",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"F7",X"A5",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",
		X"00",X"00",X"00",X"00",X"11",X"FC",X"7D",X"F4",X"7F",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"FF",X"0C",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"08",X"02",
		X"0E",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FE",X"FF",X"F6",X"FF",X"FF",X"8F",X"00",X"11",X"ED",X"ED",X"FC",X"B7",X"F3",X"C3",
		X"08",X"00",X"00",X"00",X"11",X"F8",X"7D",X"F4",X"3F",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"EF",X"0C",X"00",X"00",X"00",X"00",X"11",X"FF",X"FF",X"EF",X"0C",X"11",
		X"8F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"FF",X"CF",X"0C",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FD",X"8F",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",
		X"0C",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"BF",X"BF",X"FA",X"FD",X"8F",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"8F",X"0C",X"00",X"00",X"00",X"00",X"33",X"FF",X"FF",X"8F",X"0C",X"33",
		X"CF",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"11",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"0C",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"D3",X"D3",X"FF",X"FF",X"8F",X"00",X"00",
		X"33",X"FF",X"F7",X"8F",X"0F",X"0C",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"EF",X"0C",X"77",
		X"EF",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"0C",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"0E",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FB",X"FB",X"FF",X"FF",X"8F",X"00",X"00",
		X"33",X"FF",X"FF",X"EF",X"0F",X"08",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"EF",X"0E",X"77",
		X"EF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"74",X"C2",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"EF",X"FF",X"FF",X"0F",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",
		X"0E",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"00",X"00",
		X"11",X"FF",X"FF",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"DE",X"FF",
		X"E9",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"EF",X"7F",X"F6",X"4B",X"00",X"77",X"D3",X"DB",X"DB",X"DB",X"DB",X"F8",
		X"0E",X"00",X"00",X"00",X"11",X"B6",X"6D",X"FB",X"B7",X"FF",X"FF",X"F6",X"F6",X"8F",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"CF",X"0C",X"00",X"00",X"00",X"00",X"FC",X"FD",X"F9",X"FB",X"FF",X"FC",
		X"F9",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"0E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"EF",X"FF",X"FF",X"8F",X"00",X"77",X"DB",X"DB",X"DB",X"DB",X"DB",X"E9",
		X"0E",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"CF",X"0C",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FC",
		X"FF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"EF",X"FF",X"FF",X"0F",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",
		X"0C",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",
		X"33",X"FF",X"FF",X"FF",X"CF",X"0C",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"77",X"FF",X"CF",X"FF",X"FF",X"0E",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",
		X"0C",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"BF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",
		X"77",X"FF",X"FF",X"FF",X"EF",X"0E",X"00",X"00",X"00",X"00",X"FE",X"FA",X"FA",X"FA",X"DF",X"FF",
		X"FB",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"CE",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"77",X"FF",X"3F",X"FF",X"EF",X"0E",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",
		X"08",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"BF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",
		X"77",X"FF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"BF",X"F9",
		X"FF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"11",X"FF",X"FF",X"FF",X"CF",X"08",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",
		X"08",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"1F",X"FF",X"FF",X"CF",X"08",X"00",X"00",
		X"77",X"FF",X"FF",X"FF",X"8F",X"0E",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"BF",X"FF",
		X"EF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"FC",X"FC",X"FC",X"E9",X"08",X"00",X"00",X"33",X"FF",X"EF",X"2F",X"0F",X"0C",
		X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"EF",X"0F",X"FF",X"FF",X"CF",X"00",X"00",X"00",
		X"77",X"FF",X"FF",X"FF",X"0F",X"0E",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"7F",X"FF",
		X"EF",X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"FF",X"FF",X"FF",X"CF",X"08",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"EF",X"08",
		X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"F9",X"0F",X"0F",X"7F",X"FF",X"8E",X"00",X"00",X"00",
		X"77",X"FF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",X"00",X"00",X"77",X"FF",X"0F",X"0F",X"7F",X"F5",
		X"C7",X"08",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"CE",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"11",X"FF",X"FF",X"FF",X"CF",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"CF",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"3F",X"FF",X"CF",X"0E",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"3F",X"FF",
		X"CF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"8F",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"11",X"FF",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"8E",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"EF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"00",X"00",X"11",X"FF",X"FF",X"EF",X"7F",X"FF",
		X"CF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"11",X"FF",X"8F",X"08",X"00",X"00",X"00",X"00",
		X"00",X"00",X"33",X"FF",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",X"77",X"FF",X"EF",X"0E",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"EF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"CF",X"7F",X"EF",
		X"8F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"8E",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"77",X"FF",X"FF",X"FF",X"8E",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"8F",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"8F",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"CF",X"0F",X"0F",X"00",X"00",X"00",X"00",X"00",X"FF",X"7F",X"0F",X"FF",X"EF",
		X"8F",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"FF",X"33",X"8C",X"0F",X"0C",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"CF",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"CF",X"FF",X"EF",
		X"0E",X"00",X"00",X"00",X"00",X"00",X"00",X"33",X"FF",X"AA",X"1D",X"CF",X"0C",X"00",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",X"FD",X"FA",X"F5",X"CF",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"FE",X"F7",X"F8",X"E7",X"0F",X"0F",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"EF",X"0F",X"0F",
		X"0C",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"88",X"11",X"FF",X"0F",X"00",X"00",X"00",
		X"00",X"00",X"F8",X"F1",X"F8",X"F8",X"0F",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"8F",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"0F",X"0E",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"EF",X"0F",X"0F",
		X"08",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0E",X"00",X"00",
		X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"0F",X"0C",X"00",X"00",X"11",X"FF",X"FF",X"CF",X"0F",X"08",
		X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0E",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",
		X"08",X"00",X"00",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",
		X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"0F",X"0E",X"00",X"00",X"33",X"FF",X"FF",X"FF",X"0F",X"0C",
		X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"00",X"00",X"00",X"FF",X"F7",X"FD",X"FF",X"CF",
		X"08",X"00",X"00",X"00",X"00",X"00",X"76",X"F6",X"F6",X"F4",X"F2",X"F6",X"96",X"86",X"00",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"CF",X"0E",
		X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"08",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"00",X"00",X"FF",X"FF",X"F7",X"FD",X"0F",X"0F",
		X"0F",X"08",X"00",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"08",X"00",
		X"00",X"FF",X"F1",X"F8",X"FE",X"F3",X"F8",X"0F",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"EF",X"0E",
		X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"08",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",
		X"0F",X"0E",X"00",X"00",X"00",X"11",X"FA",X"FA",X"FA",X"FA",X"F5",X"F5",X"E5",X"A5",X"08",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0C",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",
		X"0F",X"0F",X"0C",X"00",X"00",X"11",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"AF",X"AF",X"08",X"00",
		X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"CF",X"0E",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",
		X"00",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0C",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"8F",X"0F",X"0C",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"0E",X"00",
		X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"8F",X"0E",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"33",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"8F",X"0C",X"00",X"00",X"77",X"FF",X"F9",X"FC",X"F0",X"F0",X"CF",X"0F",X"0F",X"0E",X"00",
		X"00",X"77",X"FF",X"FF",X"F0",X"F3",X"0F",X"0E",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",
		X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"CF",X"0C",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0E",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"00",X"11",X"FD",X"F9",X"0F",X"0F",X"0F",X"0F",
		X"0F",X"0F",X"0C",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"8F",X"0F",X"08",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"77",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"3F",X"EF",X"0F",X"08",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"EF",X"0F",X"0C",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"0F",X"0C",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"EF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"00",X"00",
		X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"0F",X"0C",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"CF",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"00",X"00",
		X"FF",X"FF",X"FF",X"EF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"0F",X"0C",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",X"00",
		X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"8F",X"0F",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"CF",X"00",X"00",
		X"FF",X"FF",X"FF",X"CF",X"0F",X"0F",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"EF",X"0F",X"0C",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"0F",X"0F",X"00");
begin
process(clk)
begin
	if rising_edge(clk) then
		data <= rom_data(to_integer(unsigned(addr)));
	end if;
end process;
end architecture;
