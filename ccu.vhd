library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

entity ccu is
  
  port (
    clk   : in std_logic;
    reset : in std_logic;

    -- exchange register ports
    xreg_address      : in  std_logic_vector(7 downto 0);
    xreg_write_data   : in  std_logic_vector(31 downto 0);
    xreg_write_enable : in  std_logic;
    xreg_read_data    : out std_logic_vector(31 downto 0);

    -- dma interface ports
    dma_read_data    : in  std_logic_vector(31 downto 0);
    dma_data_valid   : in  std_logic;
    dma_read_enable  : out std_logic;
    dma_write_enable : out std_logic;
    dma_write_data   : out std_logic_vector(31 downto 0);
    dma_address      : out std_logic_vector(31 downto 0);
    dma_burst        : out std_logic;
    dma_size         : out std_logic_vector(2 downto 0));

end ccu;

architecture array_add of ccu is

  -- exchange register related signals
  signal set_address, set_size, set_value, start : std_logic;
  signal address_r, size_r, value_r              : unsigned(31 downto 0);

  -- compute process related signals
  type   compute_state_t is (idle_s, read_s, wait_s, write_s, wait2_s);
  signal current_state, next_state        : compute_state_t;
  signal set_count, incr, set_data, ready : std_logic;
  signal data_r, count_r                  : unsigned(31 downto 0);
  
begin  -- behavioral

  xreg_process : process (address_r, ready, size_r, value_r, xreg_address,
                          xreg_write_enable)
  begin  -- process xreg_process

    -- Writing exchange registers
    set_address <= '0';
    set_size    <= '0';
    set_value   <= '0';
    start       <= '0';

    if xreg_write_enable = '1' then

      case xreg_address(3 downto 2) is
        when "00" =>
          -- writing to this address means start computing.
          start <= '1';
        when "01" =>
          set_address <= '1';
        when "10" =>
          set_size <= '1';
        when "11" =>
          set_value <= '1';
        when others => null;
      end case;

    end if;

    -- Reading exchange registers
    xreg_read_data <= (others => '0');
    case xreg_address(3 downto 2) is
      when "00" =>
        -- Reading from this address reads the ready state of the accelerator
        xreg_read_data(0) <= ready;
      when "01" =>
        xreg_read_data <= std_logic_vector(address_r);
      when "10" =>
        xreg_read_data <= std_logic_vector(size_r);
      when "11" =>
        xreg_read_data <= std_logic_vector(value_r);
      when others =>
        xreg_read_data <= (others => '0');
    end case;
    
  end process xreg_process;

  dma_write_data <= std_logic_vector(data_r);
  dma_address    <= std_logic_vector(address_r);
  dma_burst      <= '0';
  dma_size       <= "100";

  compute_process : process (count_r, current_state, dma_data_valid, size_r,
                             start)
  begin  -- process compute_process
    next_state <= current_state;
    set_count  <= '0';
    set_data   <= '0';
    incr       <= '0';

    dma_read_enable  <= '0';
    dma_write_enable <= '0';

    ready <= '0';

    case current_state is
      when idle_s =>
        ready <= '1';
        if start = '1' and size_r /= 0 then
          set_count  <= '1';
          next_state <= read_s;
        end if;
      when read_s =>
        dma_read_enable <= '1';
        next_state      <= wait_s;
      when wait_s =>
        if dma_data_valid = '1' then
          set_data   <= '1';
          next_state <= write_s;
        else
          dma_read_enable <= '1';
        end if;
      when write_s =>
        dma_write_enable <= '1';
        if dma_data_valid = '1' then
          if count_r >= size_r then
            next_state <= idle_s;
          else
            incr       <= '1';
            next_state <= wait2_s;
          end if;
        end if;
      when wait2_s =>
        if dma_data_valid = '1' then
          next_state <= read_s;
        end if;
      when others =>
        next_state <= idle_s;
    end case;
  end process compute_process;

  clk_process : process (clk)
  begin  -- process clk_process
    if rising_edge(clk) then
      if reset = '0' then
        current_state <= idle_s;
        address_r     <= (others => '0');
        size_r        <= (others => '0');
        value_r       <= (others => '0');
        count_r       <= (others => '0');
        data_r        <= (others => '0');
      else

        if set_address = '1' then
          address_r <= unsigned(xreg_write_data);
        end if;
        if set_size = '1' then
          size_r <= unsigned(xreg_write_data);
        end if;
        if set_value = '1' then
          value_r <= unsigned(xreg_write_data);
        end if;

        current_state <= next_state;

        if set_count = '1' then
          count_r <= to_unsigned(1, 32);
        end if;
        if incr = '1' then
          count_r   <= count_r + 1;
          address_r <= address_r + 4;
        end if;
        if set_data = '1' then
          data_r <= unsigned(dma_read_data) + value_r;
        end if;
      end if;
    end if;
  end process clk_process;

end array_add;
