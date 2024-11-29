-- Cpu.vhd
-- 情報電子工学総合実験(CE1)用 TeC の CPU 部分 !!! ひな形 !!!
--
-- (c)2014 - 2024 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Cpu is
  Port ( Clk     : in  std_logic;
         -- 制御
         Reset   : in  std_logic;
         Stop    : in  std_logic;
         Halt    : out std_logic;
         Li      : out std_logic;                       -- 命令フェッチ
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
           -- 入力
           Reset : in  STD_LOGIC;
           OP    : in  STD_LOGIC_vector (3 downto 0);
           Rd    : in  STD_LOGIC_vector (1 downto 0);
           Rx    : in  STD_LOGIC_vector (1 downto 0);
           Flag  : in  STD_LOGIC_vector (2 downto 0);   -- CSZ
           Stop  : in  STD_LOGIC;
           -- CPU内部の制御用に出力
           LI    : out STD_LOGIC;
           LD    : out STD_LOGIC;
           LF    : out STD_LOGIC;
           LR    : out STD_LOGIC;
           LP    : out STD_LOGIC;
           WR  : in  STD_LOGIC_vector (1 downto 0);
           SelDin  : in  STD_LOGIC;
           SelAddr    : in  STD_LOGIC_vector (1 downto 0);
           SelPC    : in  STD_LOGIC;
           AddPC    : in  STD_LOGIC;
           AddSP  : in  STD_LOGIC_vector (1 downto 0);
           -- CPU外部へ出力
           We    : out  STD_LOGIC;
           Halt  : out  STD_LOGIC
           );
  end component;

-- CPU Register
  signal G0  : std_logic_vector(7 downto 0);
  signal G1  : std_logic_vector(7 downto 0);
  signal G2  : std_logic_vector(7 downto 0);
  signal SP  : std_logic_vector(7 downto 0);

-- PSW
  signal PC  : std_logic_vector(7 downto 0);
  signal FLG : std_logic_vector(2 downto 0);            -- CSZ

-- IR
  signal OP  : std_logic_vector(3 downto 0);
  signal Rd  : std_logic_vector(1 downto 0);
  signal Rx  : std_logic_vector(1 downto 0);

-- オペコード
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

-- ここから一旦おいておく！！！ -------------------------
-- 内部バス
  signal EA    : std_logic_vector(7 downto 0); -- Effective Address
  signal RegRd : std_logic_vector(7 downto 0); -- Reg[Rd]
  signal RegRx : std_logic_vector(7 downto 0); -- Reg[Rx]
  signal Alu   : std_logic_vector(8 downto 0); -- ALU出力（キャリー付)
  signal Zero  : std_logic;                    -- ALUが0か？
  signal SftRd : std_logic_vector(8 downto 0); -- RegRdをシフトしたもの
-- ここまで一旦おいておく！！！ -------------------------

-- シーケンサーファイルは別においたほうがいいかも
-- 内部制御線（ステートマシンの出力)
-- 内部の制御信号をレジスタごとに細分化したもの？

  signal LI    : std_logic;
  signal LD    : std_logic;
  signal LF    : std_logic;
  signal LR    : std_logic;
  signal LP    : std_logic;
  signal WR  : std_logic_vector (1 downto 0);
  signal SelDin  : std_logic;
  signal SelAddr    :   std_logic_vector (1 downto 0);
  signal SelPC    :   std_logic;
  signal AddPC    :   std_logic;
  signal AddSP  :   std_logic_vector (1 downto 0);


begin
-- 仮の信号を出力しておく
  Halt <= '0';
  Addr <= "00000000";
  Dout <= "00000000";
  We   <= '0';
  Li   <= '0';

-- フラグ
  Flags <= FLG;
  
-- PC の制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      PC <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1' and DbgAin="100") then
        PC <= DbgDin;
      end if;
    end if;
  end process;
  
-- CPU レジスタの制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      G0  <= "00000000";
      G1  <= "00000000";
      G2  <= "00000000";
      SP  <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1') then
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

-- フラグの制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      FLG <= "000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1' and DbgAin="110") then
        FLG <= DbgDin(2 downto 0);
      end if;
    end if;
  end process;
  
-- デバッグ用のコンソール接続
  DbgDout <= G0 when DbgAin="000" else
             G1 when DbgAin="001" else
             G2 when DbgAin="010" else
             SP when DbgAin="011" else
             PC when DbgAin="100" else
             "00000" & FLG;

end Behavioral;