require 'io/console'

# foreground color
RESET_COLORS = "\033[0m"    # Reset everything on exit
BLACK_TEXT = "\033[30m"     # Normal text
RED_TEXT = "\033[31m"       # Error messages
GREEN_TEXT = "\033[32m"     # Title
BROWN_TEXT = "\033[33m"     # Registers name and value
BLUE_TEXT = "\033[34m"      # Help Text Headings | Prompt separator
MAGENTA_TEXT = "\033[35m"
CYAN_TEXT = "\033[36m"      # Help Text
GRAY_TEXT = "\033[37m"

def console_columns
    _, columns = IO.console.winsize
    [columns, 60].max
end

class Settings
  def initialize(settings_file)
    @settings_file = settings_file
    @hash = {}
    @hash = JSON.parse(File.read(settings_file)) if File.exist?(settings_file)
  end

  def stack= stack
    @hash['stack'] = stack
  end
  def stack
    @hash['stack']
  end

  def registers= registers
    @hash['registers'] = registers
  end
  def registers
    @hash['registers']
  end

  def base= base
    @hash['base'] = base
  end
  def base
    @hash['base']
  end

  def angle= angle
    @hash['angle'] = angle
  end
  def angle
    @hash['angle']
  end

  def write
    File.open(@settings_file,'w') {|f| f.write(@hash.to_json)}
  end
end
