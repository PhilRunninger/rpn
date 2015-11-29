require 'rspec'
require_relative 'settings'

def temp_settings_file(hash)
    @rpnrc = File.join(ENV['TMPDIR'], '.rpnrc')
    File.open(@rpnrc, 'w') {|f| f.write(hash.to_json)}
    @rpnrc
end

def settings_file_hash
    JSON.parse(File.read(@rpnrc))
end

describe Settings do
  # Default settings {{{1
  context 'default settings' do
    before(:each) do
      @settings = Settings.new temp_settings_file({})
    end
    after(:each) do
      File.delete @rpnrc
    end

    it 'initializes stack to []' do
      expect(@settings.stack).to eq([])
    end
    it 'initializes registers to {}' do
      expect(@settings.registers).to eq({})
    end
    it 'initializes base to 0' do
      expect(@settings.base).to eq(0)
    end
    it 'initializes angle to DEG' do
      expect(@settings.angle).to eq('DEG')
    end
    it 'initializes normal text color to :black' do
      expect(@settings.color_normal).to eq(:black)
    end
    it 'initializes error text color to :red' do
      expect(@settings.color_error).to eq(:red)
    end
    it 'initializes title text color to :green' do
      expect(@settings.color_title).to eq(:green)
    end
    it 'initializes register text color to :yellow' do
      expect(@settings.color_register).to eq(:yellow)
    end
    it 'initializes help heading text color to :blue' do
      expect(@settings.color_help_heading).to eq(:blue)
    end
    it 'initializes help text color to :cyan' do
      expect(@settings.color_help).to eq(:cyan)
    end
  end

  # Processor initializes using the settings file {{{1
  context 'processor initializes using the settings file' do
    before(:each) do
      @settings = Settings.new temp_settings_file({'stack'=>[1,2],
                                                   'registers'=>{'a'=>3},
                                                   'base'=>8,
                                                   'angle'=>'RAD',
                                                   'color_normal'=>:black,
                                                   'color_error'=>:red,
                                                   'color_title'=>:green,
                                                   'color_register'=>:yellow,
                                                   'color_help_heading'=>:blue,
                                                   'color_help'=>:cyan})
    end
    after(:each) do
      File.delete @rpnrc
    end

    it 'returns the stack' do
      expect(@settings.stack).to eq([1,2])
    end
    it 'returns the registers' do
      expect(@settings.registers).to_not be_nil
      expect(@settings.registers['a']).to eq(3)
    end
    it 'returns the base' do
      expect(@settings.base).to eq(8)
    end
    it 'returns the angle' do
      expect(@settings.angle).to eq('RAD')
    end
    it 'returns the color properties' do
      expect(@settings.color_normal).to eq(:black)
      expect(@settings.color_error).to eq(:red)
      expect(@settings.color_title).to eq(:green)
      expect(@settings.color_register).to eq(:yellow)
      expect(@settings.color_help_heading).to eq(:blue)
      expect(@settings.color_help).to eq(:cyan)
    end

    it 'writes changed settings to the file' do
      File.delete @rpnrc
      @settings.stack = [3,4]
      @settings.registers = {'z'=>5}
      @settings.base = 16
      @settings.angle = 'DEG'
      @settings.color_normal = :a
      @settings.color_error = :b
      @settings.color_title = :c
      @settings.color_register = :d
      @settings.color_help_heading = :e
      @settings.color_help = :f
      expect(File.exists?(@rpnrc)).to be(false)
      @settings.write
      expect(settings_file_hash).to eq(
        {'stack'=>[3,4],
         'registers'=>{'z'=>5},
         'base'=>16,
         'angle'=>'DEG',
         'color_normal'=>'a',
         'color_error'=>'b',
         'color_title'=>'c',
         'color_register'=>'d',
         'color_help_heading'=>'e',
         'color_help'=>'f'})
    end
  end

  # }}}
end
# vim:ft=ruby foldmethod=marker sw=4
