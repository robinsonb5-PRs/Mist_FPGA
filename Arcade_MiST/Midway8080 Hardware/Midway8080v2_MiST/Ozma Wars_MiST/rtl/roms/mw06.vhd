library ieee;
use ieee.std_logic_1164.all,ieee.numeric_std.all;

entity mw06 is
port (
	clk  : in  std_logic;
	addr : in  std_logic_vector(10 downto 0);
	data : out std_logic_vector(7 downto 0)
);
end entity;

architecture prom of mw06 is
	type rom is array(0 to  2047) of std_logic_vector(7 downto 0);
	signal rom_data: rom := (
		X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"6A",X"01",X"1C",X"02",X"05",X"66",X"C3",
		X"9F",X"47",X"70",X"60",X"66",X"1F",X"02",X"01",X"64",X"C3",X"89",X"47",X"70",X"6C",X"66",X"1F",
		X"02",X"01",X"64",X"C3",X"89",X"47",X"70",X"5A",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",
		X"70",X"54",X"66",X"1F",X"02",X"01",X"64",X"C3",X"89",X"47",X"70",X"4A",X"01",X"1C",X"02",X"05",
		X"66",X"C3",X"9F",X"47",X"70",X"42",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"3A",
		X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"32",X"01",X"1C",X"02",X"05",X"66",X"C3",
		X"9F",X"47",X"70",X"2C",X"66",X"1F",X"02",X"01",X"64",X"C3",X"89",X"47",X"70",X"3B",X"01",X"1C",
		X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"43",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",
		X"70",X"4B",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"5B",X"01",X"1C",X"02",X"05",
		X"66",X"C3",X"9F",X"47",X"70",X"63",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"73",
		X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"83",X"01",X"1C",X"02",X"05",X"66",X"C3",
		X"9F",X"47",X"70",X"8B",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"9B",X"01",X"1C",
		X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"A3",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",
		X"70",X"AB",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"BB",X"01",X"1C",X"02",X"05",
		X"66",X"C3",X"9F",X"47",X"70",X"CB",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"D3",
		X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"DB",X"01",X"1C",X"02",X"05",X"66",X"C3",
		X"9F",X"47",X"70",X"E3",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"E8",X"66",X"1F",
		X"02",X"01",X"64",X"C3",X"89",X"47",X"70",X"CA",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",
		X"70",X"C2",X"01",X"1C",X"02",X"05",X"66",X"C3",X"9F",X"47",X"70",X"BA",X"01",X"1C",X"02",X"05",
		X"66",X"C3",X"9F",X"47",X"D0",X"60",X"1B",X"18",X"02",X"0F",X"02",X"CD",X"62",X"44",X"01",X"FE",
		X"00",X"10",X"AF",X"00",X"04",X"01",X"EB",X"6A",X"1F",X"02",X"0B",X"10",X"01",X"FE",X"01",X"08",
		X"AF",X"00",X"04",X"01",X"EB",X"7E",X"4E",X"02",X"07",X"20",X"01",X"FE",X"01",X"04",X"AF",X"00",
		X"04",X"01",X"EB",X"8E",X"4E",X"02",X"07",X"30",X"01",X"FE",X"02",X"04",X"01",X"FE",X"01",X"03",
		X"CD",X"83",X"49",X"01",X"FE",X"00",X"08",X"01",X"FE",X"FF",X"04",X"CD",X"8F",X"49",X"01",X"FE",
		X"FE",X"20",X"76",X"7A",X"78",X"4A",X"1F",X"01",X"05",X"66",X"01",X"FD",X"00",X"20",X"76",X"62",
		X"74",X"4A",X"1F",X"01",X"05",X"66",X"C3",X"8A",X"49",X"D0",X"A0",X"1B",X"18",X"02",X"0F",X"02",
		X"CD",X"CA",X"44",X"CD",X"F6",X"44",X"01",X"FE",X"00",X"10",X"AF",X"00",X"04",X"01",X"EB",X"6A",
		X"1F",X"02",X"0B",X"10",X"01",X"FE",X"01",X"08",X"AF",X"00",X"04",X"01",X"EB",X"7E",X"4E",X"02",
		X"07",X"20",X"01",X"FE",X"01",X"04",X"AF",X"00",X"04",X"01",X"EB",X"8E",X"4E",X"02",X"07",X"30",
		X"01",X"FE",X"02",X"03",X"CD",X"E9",X"49",X"01",X"FE",X"01",X"04",X"01",X"FE",X"00",X"04",X"CD",
		X"F3",X"49",X"01",X"FE",X"00",X"04",X"C3",X"A2",X"44",X"82",X"73",X"4A",X"1F",X"01",X"05",X"66",
		X"C3",X"8A",X"49",X"72",X"67",X"4A",X"1F",X"01",X"05",X"66",X"C3",X"8A",X"49",X"D0",X"78",X"DB",
		X"18",X"02",X"0F",X"33",X"CD",X"61",X"4A",X"01",X"FF",X"00",X"10",X"CD",X"3F",X"4A",X"01",X"FF",
		X"00",X"10",X"01",X"FF",X"01",X"11",X"01",X"FE",X"FF",X"08",X"01",X"FD",X"FF",X"05",X"AF",X"F0",
		X"FF",X"02",X"01",X"00",X"00",X"04",X"CD",X"4B",X"4A",X"CD",X"57",X"4A",X"01",X"00",X"00",X"12",
		X"CD",X"4B",X"4A",X"CD",X"57",X"4A",X"01",X"FE",X"00",X"10",X"01",X"FD",X"FF",X"10",X"76",X"B8",
		X"7F",X"4A",X"1F",X"01",X"05",X"66",X"01",X"FD",X"00",X"40",X"76",X"50",X"7A",X"66",X"1F",X"02",
		X"01",X"64",X"01",X"FC",X"00",X"10",X"76",X"50",X"84",X"66",X"1F",X"02",X"01",X"64",X"C3",X"52",
		X"4A",X"D0",X"48",X"1B",X"19",X"02",X"0F",X"33",X"AF",X"00",X"00",X"10",X"CD",X"DF",X"4A",X"01",
		X"FF",X"00",X"10",X"CD",X"AB",X"4A",X"01",X"FF",X"00",X"11",X"01",X"FE",X"00",X"08",X"01",X"FD",
		X"00",X"05",X"AF",X"F0",X"00",X"02",X"01",X"00",X"00",X"06",X"CD",X"B5",X"4A",X"CD",X"C1",X"4A",
		X"01",X"00",X"00",X"06",X"CD",X"B5",X"4A",X"CD",X"C1",X"4A",X"01",X"FE",X"00",X"08",X"CD",X"CB",
		X"4A",X"CD",X"D5",X"4A",X"01",X"00",X"00",X"10",X"C3",X"9E",X"4A",X"B8",X"4F",X"4A",X"1F",X"01",
		X"05",X"66",X"C3",X"46",X"4A",X"60",X"48",X"40",X"1F",X"01",X"03",X"64",X"01",X"FE",X"00",X"30",
		X"76",X"60",X"54",X"40",X"1F",X"01",X"03",X"64",X"C3",X"BC",X"4A",X"50",X"48",X"40",X"1F",X"01",
		X"03",X"64",X"C3",X"BC",X"4A",X"50",X"54",X"40",X"1F",X"01",X"03",X"64",X"C3",X"BC",X"4A",X"D0",
		X"78",X"DB",X"18",X"02",X"0F",X"33",X"AF",X"00",X"00",X"10",X"01",X"FF",X"00",X"11",X"01",X"FE",
		X"00",X"08",X"01",X"FD",X"00",X"05",X"AF",X"F0",X"00",X"02",X"01",X"00",X"00",X"10",X"01",X"FE",
		X"00",X"09",X"01",X"FD",X"00",X"06",X"CD",X"13",X"4B",X"CD",X"1D",X"4B",X"01",X"00",X"00",X"10",
		X"C3",X"06",X"4B",X"40",X"79",X"66",X"1F",X"02",X"01",X"64",X"C3",X"52",X"4A",X"40",X"83",X"66",
		X"1F",X"02",X"01",X"64",X"C3",X"52",X"4A",X"D0",X"A8",X"FB",X"18",X"02",X"0F",X"33",X"CD",X"87",
		X"4B",X"01",X"FF",X"00",X"18",X"CD",X"69",X"4B",X"01",X"FF",X"00",X"08",X"01",X"FF",X"01",X"11",
		X"01",X"FE",X"FF",X"08",X"01",X"FD",X"FF",X"05",X"AF",X"F0",X"FF",X"02",X"01",X"00",X"00",X"02",
		X"CD",X"73",X"4B",X"CD",X"7D",X"4B",X"01",X"00",X"00",X"0E",X"CD",X"73",X"4B",X"CD",X"7D",X"4B",
		X"01",X"FE",X"00",X"10",X"01",X"FD",X"01",X"10",X"76",X"B0",X"AF",X"4A",X"1F",X"01",X"05",X"66",
		X"C3",X"46",X"4A",X"50",X"AA",X"66",X"1F",X"02",X"01",X"64",X"C3",X"52",X"4A",X"50",X"B4",X"66",
		X"1F",X"02",X"01",X"64",X"C3",X"52",X"4A",X"D0",X"D8",X"DB",X"18",X"02",X"0F",X"33",X"AF",X"00",
		X"00",X"10",X"CD",X"03",X"4C",X"01",X"FF",X"00",X"18",X"CD",X"D1",X"4B",X"01",X"FF",X"00",X"09",
		X"01",X"FE",X"00",X"08",X"01",X"FD",X"00",X"05",X"AF",X"F0",X"00",X"02",X"01",X"00",X"00",X"04",
		X"CD",X"DB",X"4B",X"CD",X"E5",X"4B",X"01",X"00",X"00",X"08",X"CD",X"DB",X"4B",X"CD",X"E5",X"4B",
		X"01",X"FE",X"00",X"08",X"CD",X"EF",X"4B",X"CD",X"F9",X"4B",X"01",X"00",X"00",X"10",X"C3",X"C4",
		X"4B",X"B0",X"DF",X"4A",X"1F",X"01",X"05",X"66",X"C3",X"46",X"4A",X"60",X"D8",X"40",X"1F",X"01",
		X"03",X"64",X"C3",X"BC",X"4A",X"60",X"E4",X"40",X"1F",X"01",X"03",X"64",X"C3",X"BC",X"4A",X"50",
		X"D8",X"40",X"1F",X"01",X"03",X"64",X"C3",X"BC",X"4A",X"50",X"E4",X"40",X"1F",X"01",X"03",X"64",
		X"C3",X"BC",X"4A",X"D0",X"A8",X"FB",X"18",X"02",X"0F",X"33",X"AF",X"00",X"00",X"10",X"01",X"FF",
		X"00",X"11",X"01",X"FE",X"00",X"08",X"01",X"FD",X"00",X"05",X"AF",X"F0",X"00",X"02",X"01",X"00",
		X"00",X"10",X"01",X"FE",X"00",X"09",X"01",X"FD",X"00",X"06",X"01",X"00",X"00",X"04",X"CD",X"3B",
		X"4C",X"CD",X"45",X"4C",X"01",X"00",X"00",X"10",X"C3",X"2A",X"4C",X"40",X"A9",X"66",X"1F",X"02",
		X"01",X"64",X"C3",X"52",X"4A",X"40",X"B3",X"66",X"1F",X"03",X"01",X"64",X"C3",X"52",X"4A",X"C0",
		X"8C",X"C5",X"1B",X"02",X"0E",X"3F",X"00",X"00",X"00",X"CD",X"6C",X"4C",X"CD",X"88",X"4C",X"CD",
		X"9A",X"4C",X"CD",X"B0",X"4C",X"01",X"00",X"00",X"20",X"C3",X"56",X"4C",X"AA",X"8A",X"AD",X"1B",
		X"01",X"0A",X"08",X"01",X"FE",X"00",X"08",X"01",X"FE",X"00",X"18",X"01",X"FE",X"01",X"10",X"01",
		X"FD",X"FE",X"10",X"01",X"FC",X"02",X"10",X"76",X"AA",X"92",X"AD",X"1B",X"01",X"0A",X"08",X"01",
		X"FE",X"00",X"08",X"01",X"FE",X"00",X"18",X"C3",X"7B",X"4C",X"B4",X"8A",X"AD",X"1B",X"01",X"0A",
		X"08",X"01",X"FE",X"00",X"08",X"01",X"FE",X"FE",X"0C",X"01",X"FE",X"01",X"04",X"C3",X"7B",X"4C",
		X"B4",X"92",X"AD",X"1B",X"01",X"0A",X"08",X"01",X"FE",X"00",X"08",X"CD",X"D2",X"4C",X"CD",X"E4",
		X"4C",X"CD",X"F6",X"4C",X"CD",X"04",X"4D",X"01",X"FE",X"02",X"14",X"01",X"FE",X"00",X"04",X"C3",
		X"7B",X"4C",X"AE",X"8A",X"AD",X"1B",X"01",X"0A",X"08",X"01",X"FE",X"FE",X"11",X"01",X"FE",X"01",
		X"0E",X"C3",X"7B",X"4C",X"AE",X"92",X"AD",X"1B",X"01",X"0A",X"08",X"01",X"FE",X"02",X"18",X"01",
		X"FE",X"01",X"08",X"C3",X"7B",X"4C",X"B8",X"8A",X"AD",X"1B",X"01",X"0A",X"08",X"01",X"FE",X"FE",
		X"24",X"C3",X"7B",X"4C",X"B8",X"92",X"AD",X"1B",X"01",X"0A",X"08",X"01",X"FE",X"02",X"24",X"C3",
		X"7B",X"4C",X"3E",X"01",X"F7",X"0A",X"C3",X"E3",X"0F",X"E1",X"C3",X"E3",X"0F",X"76",X"1E",X"4D",
		X"00",X"00",X"1E",X"0C",X"3F",X"0C",X"1E",X"00",X"00",X"29",X"4D",X"00",X"00",X"00",X"00",X"F0",
		X"00",X"60",X"00",X"F8",X"01",X"7E",X"00",X"FC",X"00",X"3F",X"00",X"FC",X"00",X"7E",X"00",X"F8",
		X"01",X"60",X"00",X"F0",X"00",X"00",X"00",X"00",X"00",X"D0",X"48",X"29",X"4D",X"02",X"0F",X"12",
		X"CD",X"9C",X"4D",X"CD",X"D3",X"4D",X"CD",X"0A",X"4E",X"01",X"FE",X"FE",X"10",X"01",X"FE",X"02",
		X"10",X"CD",X"76",X"4D",X"CD",X"80",X"4D",X"CD",X"8E",X"4D",X"76",X"01",X"FE",X"FE",X"10",X"01",
		X"FE",X"02",X"10",X"C3",X"6B",X"4D",X"90",X"4B",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"6B",X"4D",
		X"93",X"48",X"1E",X"4D",X"01",X"09",X"22",X"01",X"FF",X"FE",X"10",X"C3",X"6B",X"4D",X"93",X"4E",
		X"1E",X"4D",X"01",X"09",X"22",X"01",X"FF",X"02",X"08",X"C3",X"6B",X"4D",X"D0",X"78",X"29",X"4D",
		X"02",X"0F",X"12",X"01",X"FE",X"FE",X"10",X"01",X"FE",X"02",X"10",X"CD",X"B5",X"4D",X"CD",X"BF",
		X"4D",X"CD",X"C9",X"4D",X"76",X"90",X"7B",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"6B",X"4D",X"93",
		X"78",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"87",X"4D",X"93",X"7E",X"1E",X"4D",X"01",X"09",X"22",
		X"C3",X"95",X"4D",X"D0",X"B8",X"29",X"4D",X"02",X"0F",X"12",X"01",X"FE",X"FE",X"10",X"01",X"FE",
		X"02",X"10",X"CD",X"EC",X"4D",X"CD",X"F6",X"4D",X"CD",X"00",X"4E",X"76",X"90",X"BB",X"1E",X"4D",
		X"01",X"09",X"22",X"C3",X"6B",X"4D",X"93",X"B8",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"87",X"4D",
		X"93",X"BE",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"95",X"4D",X"D0",X"E8",X"29",X"4D",X"02",X"0F",
		X"12",X"01",X"FE",X"FE",X"10",X"01",X"FF",X"02",X"10",X"CD",X"23",X"4E",X"CD",X"2D",X"4E",X"CD",
		X"37",X"4E",X"76",X"90",X"EB",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"6B",X"4D",X"93",X"E8",X"1E",
		X"4D",X"01",X"09",X"22",X"C3",X"87",X"4D",X"93",X"EE",X"1E",X"4D",X"01",X"09",X"22",X"C3",X"95",
		X"4D",X"D0",X"88",X"DB",X"18",X"02",X"0F",X"33",X"CD",X"49",X"4D",X"AF",X"00",X"00",X"20",X"CD",
		X"49",X"4D",X"01",X"00",X"00",X"04",X"AF",X"00",X"04",X"02",X"01",X"00",X"00",X"04",X"AF",X"00",
		X"FC",X"02",X"01",X"00",X"00",X"04",X"C3",X"4F",X"4E",X"D0",X"10",X"AD",X"1B",X"01",X"0A",X"08",
		X"CD",X"49",X"4D",X"AF",X"00",X"00",X"10",X"CD",X"FD",X"49",X"CD",X"27",X"4B",X"76",X"7E",X"4E",
		X"00",X"00",X"E0",X"1F",X"78",X"7C",X"FF",X"7F",X"FC",X"3F",X"F0",X"1F",X"00",X"00",X"8E",X"4E",
		X"00",X"00",X"00",X"00",X"C0",X"0F",X"FE",X"7F",X"E0",X"0F",X"00",X"00",X"00",X"00",X"9E",X"4E",
		X"00",X"00",X"40",X"00",X"E0",X"00",X"40",X"00",X"00",X"00",X"AA",X"4E",X"00",X"00",X"E0",X"00",
		X"F0",X"01",X"B0",X"01",X"F0",X"01",X"E0",X"00",X"00",X"00",X"BA",X"4E",X"00",X"00",X"E0",X"00",
		X"50",X"01",X"E8",X"02",X"B8",X"03",X"E8",X"02",X"50",X"01",X"E0",X"00",X"00",X"00",X"CE",X"4E",
		X"00",X"00",X"B0",X"01",X"F8",X"03",X"EC",X"06",X"B4",X"05",X"58",X"03",X"B4",X"05",X"EC",X"06",
		X"F8",X"03",X"B0",X"01",X"00",X"00",X"E6",X"4E",X"00",X"00",X"B0",X"01",X"F8",X"03",X"5C",X"07",
		X"AE",X"0E",X"56",X"0D",X"F8",X"03",X"56",X"0D",X"AE",X"0E",X"5C",X"07",X"F8",X"03",X"B0",X"01",
		X"00",X"00",X"C0",X"04",X"00",X"00",X"00",X"10",X"C6",X"00",X"00",X"00",X"6C",X"29",X"10",X"01",
		X"00",X"16",X"04",X"06",X"11",X"00",X"6E",X"C9",X"00",X"00",X"00",X"34",X"C2",X"05",X"90",X"00",
		X"DF",X"08",X"02",X"00",X"00",X"CC",X"25",X"08",X"02",X"10",X"BA",X"43",X"92",X"80",X"00",X"A1",
		X"4C",X"08",X"21",X"02",X"2F",X"2B",X"31",X"04",X"10",X"D0",X"6A",X"04",X"81",X"20",X"0A",X"95",
		X"02",X"10",X"02",X"A0",X"A8",X"45",X"01",X"04",X"10",X"51",X"00",X"92",X"00",X"01",X"44",X"06",
		X"20",X"01",X"36",X"92",X"44",X"42",X"00",X"C2",X"A0",X"91",X"04",X"00",X"00",X"10",X"03",X"01",
		X"02",X"28",X"08",X"0C",X"20",X"00",X"80",X"92",X"02",X"41",X"08",X"04",X"00",X"25",X"90",X"00",
		X"50",X"05",X"4A",X"24",X"00",X"00",X"20",X"D4",X"48",X"00",X"8A",X"42",X"80",X"81",X"42",X"02",
		X"10",X"00",X"21",X"04",X"40",X"01",X"20",X"02",X"08",X"04",X"48",X"44",X"00",X"00",X"20",X"10",
		X"88",X"00",X"00",X"00",X"04",X"10",X"10",X"01",X"10",X"01",X"21",X"00",X"20",X"28",X"08",X"00",
		X"40",X"44",X"80",X"00",X"04",X"02",X"00",X"00",X"14",X"00",X"04",X"00",X"20",X"20",X"01",X"09",
		X"00",X"00",X"02",X"02",X"00",X"08",X"00",X"01",X"20",X"00",X"00",X"40",X"80",X"00",X"00",X"20",
		X"00",X"10",X"80",X"00",X"40",X"00",X"20",X"00",X"00",X"80",X"FF",X"FF",X"07",X"07",X"07",X"07",
		X"07",X"07",X"07",X"07",X"07",X"02",X"03",X"04",X"05",X"06",X"12",X"2D",X"0A",X"00",X"0E",X"17",
		X"0E",X"1B",X"10",X"22",X"2E",X"18",X"1E",X"1D",X"00",X"3E",X"09",X"CD",X"05",X"0A",X"E1",X"D1",
		X"C3",X"12",X"4D",X"3E",X"E1",X"C3",X"EB",X"4F",X"F7",X"0A",X"F7",X"0C",X"AF",X"E7",X"0B",X"C9");
begin
process(clk)
begin
	if rising_edge(clk) then
		data <= rom_data(to_integer(unsigned(addr)));
	end if;
end process;
end architecture;
