require 'json'

class Settings
  attr_reader :hash

  def initialize(settings_file)
    @settings_file = settings_file
    @hash = {}
    @hash = JSON.parse(File.read(settings_file)) if File.exist?(settings_file)
  end

  def write
    File.open(@settings_file,'w') {|f| f.write(@hash.to_json)}
  end

  def stack= stack
    @hash['stack'] = stack
  end
  def stack
    @hash['stack'] || []
  end

  def registers= registers
    @hash['registers'] = registers
  end
  def registers
    @hash['registers'] || {}
  end

  def base= base
    @hash['base'] = base
  end
  def base
    @hash['base'] || 0
  end

  def angle= angle
    @hash['angle'] = angle
  end
  def angle
    @hash['angle'] || 'DEG'
  end

  def color_normal= color_normal
    @hash['color_normal'] = color_normal
  end
  def color_normal
    @hash['color_normal'] || '\033[30m'
  end

  def color_error= color_error
    @hash['color_error'] = color_error
  end
  def color_error
    @hash['color_error'] || '\033[31m'
  end

  def color_title= color_title
    @hash['color_title'] = color_title
  end
  def color_title
    @hash['color_title'] || '\033[32m'
  end

  def color_register= color_register
    @hash['color_register'] = color_register
  end
  def color_register
    @hash['color_register'] || '\033[33m'
  end

  def color_help_heading= color_help_heading
    @hash['color_help_heading'] = color_help_heading
  end
  def color_help_heading
    @hash['color_help_heading'] || '\033[34m'
  end

  def color_help= color_help
    @hash['color_help'] = color_help
  end
  def color_help
    @hash['color_help'] || '\033[36m'
  end
end

