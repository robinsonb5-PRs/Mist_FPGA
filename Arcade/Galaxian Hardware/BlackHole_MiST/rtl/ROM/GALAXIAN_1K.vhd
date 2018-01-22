library ieee;
use ieee.std_logic_1164.all,ieee.numeric_std.all;

entity GALAXIAN_1K is
port (
	clk  : in  std_logic;
	addr : in  std_logic_vector(10 downto 0);
	data : out std_logic_vector(7 downto 0)
);
end entity;

architecture prom of GALAXIAN_1K is
	type rom is array(0 to  2047) of std_logic_vector(7 downto 0);
	signal rom_data: rom := (
		X"22",X"3E",X"00",X"3E",X"2A",X"2A",X"2A",X"00",X"00",X"3E",X"22",X"22",X"3E",X"00",X"3E",X"22",
		X"22",X"3E",X"00",X"02",X"3E",X"22",X"00",X"00",X"00",X"3E",X"22",X"22",X"3E",X"00",X"3E",X"22",
		X"00",X"18",X"18",X"0C",X"0C",X"0C",X"06",X"07",X"00",X"18",X"18",X"30",X"30",X"30",X"60",X"E0",
		X"03",X"03",X"01",X"00",X"00",X"00",X"00",X"00",X"C0",X"C0",X"80",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"10",X"02",X"04",X"10",X"68",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"03",X"3F",X"7F",X"3F",X"3F",X"7F",X"3F",X"1F",X"CC",X"FC",X"FC",X"DC",X"5E",X"EE",X"EE",X"EE",
		X"1F",X"3F",X"3F",X"3F",X"3F",X"7F",X"61",X"C0",X"FC",X"FC",X"FE",X"FF",X"FE",X"FE",X"FF",X"1D",
		X"00",X"00",X"67",X"FF",X"7D",X"38",X"38",X"1C",X"03",X"36",X"FC",X"FC",X"B8",X"18",X"0E",X"1F",
		X"18",X"1C",X"3C",X"3F",X"3F",X"6F",X"46",X"80",X"1C",X"1C",X"3C",X"FE",X"FE",X"C7",X"01",X"00",
		X"80",X"60",X"70",X"38",X"3F",X"1F",X"1F",X"0F",X"02",X"07",X"0F",X"FF",X"FF",X"FF",X"FB",X"F9",
		X"06",X"0F",X"0F",X"1F",X"1F",X"1F",X"3F",X"7F",X"70",X"00",X"80",X"C0",X"80",X"01",X"03",X"07",
		X"08",X"1C",X"FE",X"FF",X"FF",X"F7",X"E7",X"82",X"21",X"76",X"FE",X"FE",X"FC",X"BC",X"3E",X"7F",
		X"00",X"00",X"18",X"7C",X"FC",X"FE",X"FC",X"F0",X"7F",X"3F",X"0E",X"0E",X"1E",X"3C",X"3C",X"78",
		X"FF",X"3F",X"1F",X"1F",X"1F",X"1F",X"0E",X"0E",X"87",X"CF",X"C6",X"C2",X"80",X"C0",X"10",X"FF",
		X"0F",X"1F",X"1F",X"1F",X"1C",X"38",X"60",X"80",X"FF",X"FF",X"FF",X"C7",X"07",X"03",X"03",X"01",
		X"E0",X"C0",X"00",X"00",X"00",X"18",X"FF",X"FF",X"78",X"70",X"70",X"70",X"38",X"18",X"1C",X"FC",
		X"FF",X"FF",X"F3",X"E1",X"C0",X"80",X"80",X"00",X"FE",X"FE",X"FE",X"FF",X"FF",X"1F",X"00",X"00",
		X"00",X"00",X"00",X"00",X"01",X"03",X"02",X"00",X"00",X"00",X"00",X"00",X"C0",X"E0",X"A0",X"80",
		X"00",X"00",X"01",X"03",X"07",X"67",X"65",X"FD",X"80",X"80",X"C0",X"E0",X"F0",X"F0",X"DF",X"DF",
		X"FD",X"65",X"66",X"07",X"03",X"01",X"00",X"00",X"DF",X"DF",X"30",X"F0",X"E0",X"C0",X"80",X"80",
		X"00",X"02",X"03",X"01",X"00",X"00",X"00",X"00",X"80",X"A0",X"E0",X"C0",X"00",X"00",X"00",X"00",
		X"F0",X"F8",X"F8",X"FC",X"FC",X"FE",X"FE",X"FF",X"00",X"00",X"00",X"01",X"03",X"07",X"03",X"03",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"03",X"87",X"8F",X"FF",X"FF",X"FE",X"F0",X"E0",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"E0",X"F0",X"FE",X"FF",X"FF",X"8F",X"87",X"03",
		X"FF",X"FE",X"FE",X"FC",X"FC",X"F8",X"F8",X"F0",X"03",X"03",X"07",X"03",X"01",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"C0",X"00",X"00",X"00",X"00",X"00",X"01",X"97",X"F0",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"97",X"01",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"2F",X"00",X"7F",X"FF",X"FF",X"FF",X"01",X"00",X"C0",X"00",X"C0",X"80",X"80",X"F0",X"F0",
		X"FF",X"FF",X"FF",X"7F",X"00",X"2F",X"00",X"00",X"F0",X"80",X"80",X"C0",X"00",X"C0",X"00",X"00",
		X"00",X"00",X"00",X"00",X"01",X"03",X"07",X"FF",X"00",X"00",X"00",X"00",X"F8",X"F8",X"F8",X"FF",
		X"07",X"03",X"01",X"00",X"00",X"00",X"00",X"00",X"F8",X"F8",X"F8",X"00",X"00",X"00",X"00",X"00",
		X"1F",X"30",X"1F",X"00",X"00",X"0F",X"00",X"FE",X"E0",X"20",X"E0",X"00",X"00",X"00",X"00",X"00",
		X"00",X"0F",X"00",X"00",X"1F",X"30",X"1F",X"00",X"00",X"00",X"00",X"00",X"E0",X"20",X"E0",X"00",
		X"71",X"FF",X"FF",X"FF",X"7F",X"7E",X"3C",X"1C",X"17",X"CF",X"DF",X"FF",X"FF",X"FB",X"21",X"00",
		X"1E",X"1F",X"7F",X"FF",X"F9",X"F8",X"F8",X"7C",X"00",X"00",X"86",X"CF",X"CF",X"0F",X"1F",X"70",
		X"00",X"18",X"B8",X"FC",X"FF",X"FF",X"FF",X"1F",X"1F",X"3F",X"7E",X"F8",X"F0",X"F0",X"C0",X"80",
		X"0F",X"07",X"03",X"03",X"83",X"87",X"07",X"07",X"E0",X"E0",X"E0",X"E0",X"E0",X"F0",X"F0",X"78",
		X"1C",X"1C",X"1E",X"3E",X"7E",X"FF",X"FF",X"E7",X"FF",X"7F",X"3C",X"30",X"00",X"00",X"00",X"E0",
		X"E3",X"C7",X"EF",X"1F",X"3C",X"78",X"F0",X"80",X"F1",X"FF",X"FF",X"FF",X"7F",X"7E",X"3C",X"10",
		X"02",X"80",X"00",X"08",X"08",X"18",X"38",X"78",X"38",X"3C",X"3F",X"7F",X"FE",X"60",X"60",X"E0",
		X"FC",X"FF",X"FF",X"FF",X"FF",X"1F",X"0F",X"06",X"F0",X"F0",X"F8",X"FC",X"FE",X"C7",X"83",X"00",
		X"00",X"00",X"3C",X"3C",X"3C",X"3C",X"00",X"00",X"82",X"C6",X"6E",X"3C",X"18",X"FE",X"FE",X"00",
		X"00",X"00",X"3C",X"3C",X"3C",X"3C",X"00",X"00",X"1F",X"11",X"11",X"1F",X"00",X"1F",X"11",X"11",
		X"38",X"7C",X"C2",X"82",X"86",X"7C",X"38",X"00",X"02",X"02",X"FE",X"FE",X"42",X"02",X"00",X"00",
		X"62",X"F2",X"BA",X"9A",X"9E",X"CE",X"46",X"00",X"8C",X"DE",X"F2",X"B2",X"92",X"86",X"04",X"00",
		X"08",X"FE",X"FE",X"C8",X"68",X"38",X"18",X"00",X"1C",X"BE",X"A2",X"A2",X"A2",X"E6",X"E4",X"00",
		X"0C",X"9E",X"92",X"92",X"D2",X"7E",X"3C",X"00",X"C0",X"E0",X"B0",X"9E",X"8E",X"C0",X"C0",X"00",
		X"0C",X"6E",X"9A",X"9A",X"B2",X"F2",X"6C",X"00",X"78",X"FC",X"96",X"92",X"92",X"F2",X"60",X"00",
		X"3E",X"7E",X"C8",X"88",X"C8",X"7E",X"3E",X"00",X"44",X"C6",X"82",X"82",X"C6",X"7C",X"38",X"00",
		X"38",X"7C",X"C6",X"82",X"82",X"FE",X"FE",X"00",X"82",X"92",X"92",X"92",X"FE",X"FE",X"00",X"00",
		X"FE",X"FE",X"10",X"10",X"10",X"FE",X"FE",X"00",X"82",X"82",X"FE",X"FE",X"82",X"82",X"00",X"00",
		X"02",X"02",X"02",X"02",X"FE",X"FE",X"00",X"00",X"FE",X"FE",X"1C",X"38",X"70",X"FE",X"FE",X"00",
		X"7C",X"FE",X"82",X"82",X"82",X"FE",X"7C",X"00",X"70",X"F8",X"88",X"88",X"88",X"FE",X"FE",X"00",
		X"72",X"F6",X"9E",X"8C",X"88",X"FE",X"FE",X"00",X"0C",X"5E",X"D2",X"92",X"92",X"F6",X"64",X"00",
		X"80",X"80",X"FE",X"FE",X"80",X"80",X"00",X"00",X"6C",X"FE",X"92",X"92",X"92",X"FE",X"FE",X"00",
		X"C0",X"F0",X"1E",X"1E",X"F0",X"C0",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"7F",X"FF",X"F0",X"F0",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FC",X"FF",X"07",X"07",X"FF",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"80",X"80",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",
		X"F8",X"FC",X"3C",X"3C",X"FC",X"FC",X"FC",X"FC",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"1F",X"3F",
		X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"F0",X"F8",
		X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"3F",X"7F",X"FE",X"FE",X"FE",X"FE",X"00",X"FF",X"FF",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"00",X"E0",X"F0",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"1F",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"80",X"1E",X"1E",X"1E",X"1E",X"1E",X"1E",X"1E",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"3F",X"7F",X"FF",X"F0",X"F0",X"FF",X"7F",X"3F",
		X"FF",X"FF",X"FF",X"03",X"03",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"C0",X"C0",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"80",X"80",X"80",X"00",X"00",X"80",X"80",X"80",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"F0",X"FF",X"7F",X"3F",X"1F",X"00",X"00",X"1F",X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",
		X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"F0",X"F0",X"E0",X"C0",X"80",X"00",X"00",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"00",X"00",X"1F",X"3F",X"7F",X"FF",X"F0",
		X"FF",X"00",X"00",X"E0",X"E0",X"E0",X"E0",X"00",X"FF",X"00",X"00",X"7F",X"7F",X"7F",X"7F",X"00",
		X"00",X"00",X"00",X"80",X"C0",X"E0",X"F0",X"F0",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"C3",X"E7",X"FF",X"3C",X"3C",X"FF",X"FF",X"FF",
		X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"07",X"07",X"07",X"07",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"81",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"07",X"07",X"07",X"07",X"00",X"00",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"F0",X"F0",
		X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"1F",X"3F",X"3C",X"3C",X"3F",X"1F",X"0F",X"07",
		X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",
		X"FE",X"FF",X"0F",X"0F",X"FF",X"FE",X"FC",X"F8",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"07",X"07",X"07",X"07",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"07",X"0F",
		X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"FF",X"FF",
		X"FF",X"FF",X"FF",X"FF",X"00",X"00",X"F8",X"FC",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"FF",X"00",X"00",X"00",X"00",X"00",X"00",X"00",
		X"FF",X"00",X"00",X"0F",X"0F",X"0F",X"0F",X"0F",X"F0",X"F0",X"F0",X"F0",X"F0",X"FF",X"FF",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"3C",X"3C",X"3C",X"3C",X"3C",X"FF",X"FF",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"FF",X"FF",X"FF",X"0F",X"0F",X"0F",X"0F",X"0F",X"FF",X"FF",X"FF",
		X"00",X"00",X"00",X"00",X"00",X"01",X"03",X"03",X"00",X"00",X"00",X"00",X"00",X"80",X"C0",X"C0",
		X"07",X"06",X"0C",X"0C",X"0C",X"18",X"18",X"00",X"E0",X"60",X"30",X"30",X"30",X"18",X"18",X"00",
		X"00",X"00",X"00",X"00",X"01",X"03",X"07",X"FF",X"00",X"00",X"00",X"00",X"F8",X"F8",X"F8",X"FF",
		X"07",X"03",X"01",X"00",X"00",X"00",X"00",X"00",X"F8",X"F8",X"F8",X"00",X"00",X"00",X"00",X"00",
		X"1F",X"30",X"1F",X"00",X"00",X"0F",X"00",X"FE",X"E0",X"20",X"E0",X"00",X"00",X"00",X"00",X"00",
		X"00",X"0F",X"00",X"00",X"1F",X"30",X"1F",X"00",X"00",X"00",X"00",X"00",X"E0",X"E0",X"E0",X"00",
		X"00",X"00",X"00",X"00",X"01",X"03",X"06",X"3E",X"00",X"00",X"00",X"00",X"F8",X"F9",X"AF",X"AF",
		X"06",X"03",X"01",X"00",X"00",X"00",X"00",X"00",X"0F",X"F9",X"F8",X"00",X"00",X"00",X"00",X"00",
		X"1F",X"1F",X"1F",X"7F",X"FF",X"F0",X"FF",X"FF",X"E0",X"E0",X"E0",X"C0",X"80",X"80",X"F0",X"F0",
		X"FF",X"F0",X"FF",X"7F",X"1F",X"1F",X"1F",X"00",X"F0",X"80",X"80",X"C0",X"E0",X"E0",X"E0",X"00",
		X"7F",X"7F",X"3F",X"1F",X"0F",X"07",X"03",X"01",X"07",X"07",X"03",X"03",X"01",X"01",X"00",X"00",
		X"3F",X"3F",X"1F",X"1F",X"1F",X"0F",X"0F",X"0F",X"FF",X"7F",X"7F",X"7F",X"7F",X"7F",X"3F",X"3F",
		X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"FF",X"3F",X"3F",X"7F",X"7F",X"7F",X"7F",X"7F",X"FF",
		X"0F",X"0F",X"0F",X"1F",X"1F",X"1F",X"3F",X"3F",X"00",X"00",X"01",X"01",X"03",X"03",X"07",X"07",
		X"01",X"03",X"07",X"0F",X"1F",X"3F",X"7F",X"7F",X"00",X"68",X"10",X"04",X"02",X"10",X"00",X"00",
		X"00",X"00",X"10",X"02",X"04",X"10",X"68",X"00",X"18",X"92",X"08",X"0C",X"48",X"10",X"08",X"18",
		X"9E",X"9E",X"92",X"82",X"C6",X"7C",X"38",X"00",X"FC",X"FE",X"02",X"02",X"02",X"FE",X"FC",X"00",
		X"80",X"90",X"90",X"90",X"FE",X"FE",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00",X"00");
begin
process(clk)
begin
	if rising_edge(clk) then
		data <= rom_data(to_integer(unsigned(addr)));
	end if;
end process;
end architecture;
