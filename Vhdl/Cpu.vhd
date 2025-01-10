-- Cpu.vhd
-- (CE1) TeC  CPU  !!!  !!!
--
-- (c)2014 - 2024 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Cpu is
  Port ( Clk     : in  std_logic;
         -- 
         Reset   : in  std_logic;
         Stop    : in  std_logic;
         Halt    : out std_logic;
         Li      : out std_logic;                       -- 
         Flags   : out std_logic_vector (2 downto 0);   -- CSZ
         -- RAM
         Addr    : out std_logic_vector (7 downto 0);
         Din     : in  std_logic_vector (7 downto 0);
         Dout    : out std_logic_vector (7 downto 0);
         We      : out std_logic;
         -- Console
         DbgAin  : in  std_logic_vector (2 downto 0);
         DbgDin  : in  std_logic_vector (7 downto 0);
         DbgDout : out std_logic_vector (7 downto 0);
         DbgWe   : in  std_logic
         );
end Cpu;
architecture Behavioral of Cpu is
  component Sequencer is
    Port ( Clk   : in  STD_LOGIC;
           -- 
           Reset : in  STD_LOGIC;
           OP    : in  STD_LOGIC_vector (3 downto 0);
           Rd    : in  STD_LOGIC_vector (1 downto 0);
           Rx    : in  STD_LOGIC_vector (1 downto 0);
           Flag  : in  STD_LOGIC_vector (2 downto 0);   -- CSZ
           Stop  : in  STD_LOGIC;
           -- CPU
           LIC    : out STD_LOGIC;
           LD    : out STD_LOGIC;
           LF    : out STD_LOGIC;
           LR    : out STD_LOGIC;
           LP    : out STD_LOGIC;
           WR  : out  STD_LOGIC_vector (1 downto 0);
           SelDin  : out  STD_LOGIC;
           SelAddr    : out  STD_LOGIC_vector (1 downto 0);
           SelPC    : out  STD_LOGIC;
			  SelSP    : out STD_LOGIC;
           AddPC    : out  STD_LOGIC;
           AddSP  : out  STD_LOGIC;
           -- CPU
           We    : out  STD_LOGIC;
           Halt  : out  STD_LOGIC
           );
  end component;

-- CPU Register
  signal G0  : std_logic_vector(7 downto 0);
  signal G1  : std_logic_vector(7 downto 0);
  signal G2  : std_logic_vector(7 downto 0);
  signal SP  : std_logic_vector(7 downto 0);
-- Calculation variable
  signal SP_c : std_logic_vector(7 downto 0);

-- PSW
  signal PC  : std_logic_vector(7 downto 0);
  signal FLG : std_logic_vector(2 downto 0);            -- CSZ

-- IR
  signal OP  : std_logic_vector(3 downto 0);
  signal Rd  : std_logic_vector(1 downto 0);
  signal Rx  : std_logic_vector(1 downto 0);

-- 
-- STCK: PUSH, POP, SFT: shift
  constant OP_NO  : std_logic_vector(3 downto 0) := "0000"; -- 0
  constant OP_LD  : std_logic_vector(3 downto 0) := "0001"; -- 1
  constant OP_ST  : std_logic_vector(3 downto 0) := "0010"; -- 2
  constant OP_ADD : std_logic_vector(3 downto 0) := "0011"; -- 3
  constant OP_SUB : std_logic_vector(3 downto 0) := "0100"; -- 4
  constant OP_CMP : std_logic_vector(3 downto 0) := "0101"; -- 5
  constant OP_AND : std_logic_vector(3 downto 0) := "0110"; -- 6
  constant OP_OR  : std_logic_vector(3 downto 0) := "0111"; -- 7
  constant OP_XOR : std_logic_vector(3 downto 0) := "1000"; -- 8
  constant OP_SFT : std_logic_vector(3 downto 0) := "1001"; -- 9
  constant OP_JMP : std_logic_vector(3 downto 0) := "1010"; -- A
  constant OP_CALL: std_logic_vector(3 downto 0) := "1011"; -- B
  constant OP_STCK: std_logic_vector(3 downto 0) := "1101"; -- D
  constant OP_RET : std_logic_vector(3 downto 0) := "1110"; -- E
  constant OP_HALT: std_logic_vector(3 downto 0) := "1111"; -- F

-- DR
  signal DR  : std_logic_vector(7 downto 0);

--  -------------------------
-- 
  signal EA    : std_logic_vector(7 downto 0); -- Effective Address
  signal RegRd : std_logic_vector(7 downto 0); -- Reg[Rd]
  signal RegRx : std_logic_vector(7 downto 0); -- Reg[Rx]
  signal Alu   : std_logic_vector(8 downto 0); -- ALU)
  signal Zero  : std_logic;                    -- ALU0
  signal SftRd : std_logic_vector(8 downto 0); -- RegRd
--  -------------------------

-- 
-- )
-- 
  -- input signal from control device
  -- if the signal is not vector, it means enable signal.
  signal LIC      : std_logic;                        -- Load Insraction
  signal LD      : std_logic;                         -- Load Data
  signal LF      : std_logic;                         -- Load Flag
  signal LR      : std_logic;                         -- Load Register
  signal LP      : std_logic;                         -- Load Program counter
  signal WR      : std_logic_vector (1 downto 0);     -- Write Register
  signal SelDin  : std_logic;                         -- Select Datain (BUS)
  signal SelAddr : std_logic_vector (1 downto 0);     -- Select Address (BUS)
  signal SelPC   : std_logic;                         -- Select Program Counter
  signal SelSP   : std_logic;                         -- Select Stack Pointer (select value ALU or result of ADD)
  signal AddPC   : std_logic;                         -- Add Program Counter (select value 0 or 1)
  signal AddSP   : std_logic;                         -- Add Stack Pointer (select value -1 or 1)


begin
-- 
  --Halt <= '0';
  --Addr <= "00000000";
  --Dout <= "00000000";
  --We   <= '0';
  --Li   <= '0';

-- 
  Flags <= FLG;
  Li <= LIC;

-- 
  seq1: Sequencer Port map (Clk, Reset, OP, Rd, Rx, FLG, Stop,
                            LIC, LD, LF, LR, LP, WR, SelDin, SelAddr,
                            SelPC, AddPC, AddSP, SelSP, We, Halt);
-- BUS
--   
  Addr <= EA when SelAddr="00" else
          SP_c when SelAddr="01" else
          SP when SelAddr="10" else PC;

-- EA  
  EA <= DR + RegRx;

-- Dout   
  Dout <= RegRd when SelDin = '0' else PC;

-- ALU 

-- 
  SftRd <= (RegRd & '0') when Rx(1)='0' else                    -- SHLA/SHLL
  (RegRd(0) & RegRd(7) & RegRd(7 downto 1)) when Rx(0)='0' else -- SHRA
  (RegRd(0) & '0' & RegRd(7 downto 1));                         -- SHRL

-- OP  ALU 
  Alu <= ('0' & DR) when OP=OP_LD else
         ('0' & RegRd) + ('0' & DR) when OP=OP_ADD else
         ('0' & RegRd) - ('0' & DR) when OP=OP_SUB or OP=OP_CMP else
         ('0' & RegRd)and('0' & DR) when OP=OP_AND else
         ('0' & RegRd)or ('0' & DR) when OP=OP_OR  else
         ('0' & RegRd)xor('0' & DR) when OP=OP_XOR else
         SftRd when OP=OP_SFT else ('0' & DR); 

  Zero <= '1' when ALU(7 downto 0)="00000000" else '0';

-- IR,DR 
process(Clk)
begin
  if (Clk'event and Clk='1') then
    if (LIC='1') then
      OP <= Din(7 downto 4);
      Rd <= Din(3 downto 2);
      Rx <= Din(1 downto 0);
    end if;
    if (LD='1') then
      DR <= Din;
    end if;
  end if;
end process;

-- PC 
  process(Clk, Reset,LP)
  begin
    if (Reset='1') then
      PC <= "00000000";
    elsif (Clk'event and Clk='1' and LP = '1')  then
      if (SelPC ='0') then
        PC <= EA;
      elsif (AddPC = '1') then
        PC <= PC + 1;
      elsif (DbgWe='1' and DbgAin="100") then
        PC <= DbgDin;
      end if;
    end if;
  end process;
  
-- CPU 
  -- GR
  RegRd <= G0 when Rd="00" else G1 when Rd="01" else
           G2 when Rd="10" else SP;
           
  -- XR
  RegRx <= G1 when Rx="01" else G2 when Rx="10" else "00000000";

  process(Clk, Reset)
  begin
    if (Reset='1') then
      G0  <= "00000000";
      G1  <= "00000000";
      G2  <= "00000000";
      SP  <= "00000000";
    elsif (Clk'event and Clk='1') then

	 -- Write to the register.
    if (LR='1') then
        case WR is
          when "00" => G0 <= Alu(7 downto 0);
          when "01" => G1 <= Alu(7 downto 0);
          when "10" => G2 <= Alu(7 downto 0);
          when others => 
						if (SelSP='0') then
							SP <= Alu(7 downto 0);
						else
							SP <= SP_c;
						end if;
        end case;
      elsif (AddSP='0') then
        SP_c <= SP + 1;
      elsif (AddSP='1') then
        SP_c <= SP - 1;
      elsif (DbgWe='1') then
        case DbgAin is
          when "000" => G0 <= DbgDin;
          when "001" => G1 <= DbgDin;
          when "010" => G2 <= DbgDin;
          when "011" => SP <= DbgDin;
          when others => null;
        end case;
      end if;
    end if;
  end process;

-- 
  process(Clk, Reset)
  begin
    if (Reset='1') then
      FLG <= "000";
    elsif (Clk'event and Clk='1') then
      -- ALU
      if (LF='1') then
        FLG(2) <= Alu(8);                -- Carry
        FLG(1) <= Alu(7);                -- Sign
        FLG(0) <= Zero;                  -- Zero
      elsif (DbgWe='1' and DbgAin="110") then
        FLG <= DbgDin(2 downto 0);
      end if;
    end if;
  end process;
  
-- 
  DbgDout <= G0 when DbgAin="000" else
             G1 when DbgAin="001" else
             G2 when DbgAin="010" else
             SP when DbgAin="011" else
             PC when DbgAin="100" else
             "00000" & FLG;

end Behavioral;