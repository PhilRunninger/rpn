require 'rspec'
require_relative 'number'
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
  # When working with an empty settings file {{{1
  context 'when working with an empty settings file,' do
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
    it 'initializes macros to {}' do
      expect(@settings.macros).to eq({})
    end
    it 'initializes base to 0' do
      expect(@settings.base).to eq(0)
    end
    it 'initializes angle to DEG' do
      expect(@settings.angle).to eq('DEG')
    end
    it 'initializes normal text color to :default' do
      expect(@settings.color_normal).to eq(:default)
    end
    it 'initializes error text color to :default' do
      expect(@settings.color_error).to eq(:default)
    end
    it 'initializes title text color to :default' do
      expect(@settings.color_title).to eq(:default)
    end
    it 'initializes register text color to :default' do
      expect(@settings.color_register).to eq(:default)
    end
    it 'initializes help heading text color to :default' do
      expect(@settings.color_help_heading).to eq(:default)
    end
    it 'initializes help text color to :default' do
      expect(@settings.color_help).to eq(:default)
    end
  end

  # When working with a non-empty settings file {{{1
  context 'when working with a non-empty settings file,' do
    before(:each) do
      @settings = Settings.new temp_settings_file({'stack'=>[Number.new("1").to_h, Number.new("02").to_h],
                                                   'registers'=>{'a'=>Number.new("0x3").to_h},
                                                   'macros'=>{'f'=>['3','*']},
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
      expect(@settings.stack).to eq([Number.new("1"), Number.new("02")])
    end
    it 'returns the registers' do
      expect(@settings.registers).to_not be_nil
      expect(@settings.registers['a']).to eq(Number.new("0x3"))
    end
    it 'stores an array of Number objects in a single register' do
      @settings.registers = {'z'=>[Number.new("0x5"), Number.new("0b101")]}
      expect(@settings.registers['z']).to eq([Number.new("0x5"), Number.new("0b101")])
    end

    it 'returns the macros' do
        expect(@settings.macros).to eq({'f'=>['3','*']})
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
      @settings.stack = [Number.new("3"),Number.new("04")]
      @settings.registers = {'z'=>Number.new("0x5")}
      @settings.macros = {'f'=>['4','/']}
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
        {'stack'=>[{"value"=>3.0, "base_as_entered"=>0},{"value"=>4.0, "base_as_entered"=>8}],
         'registers'=>{'z'=>{"value"=>5.0, "base_as_entered"=>16}},
         'macros'=>{'f'=>['4','/']},
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