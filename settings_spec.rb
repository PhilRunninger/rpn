require 'rspec'
require_relative 'number'
require_relative 'settings'

def temp_settings_file(hash)
  @rpnrc = File.join(ENV['TMPDIR'], '.rpnrc')
  File.open(@rpnrc, 'w') {|f| f.write(hash.to_json)}
  @rpnrc
end

def empty_settings_file()
  @rpnrc = File.join(ENV['TMPDIR'], '.rpnrc')
  File.open(@rpnrc, 'w') {}
  @rpnrc
end

def settings_file_hash
  JSON.parse(File.read(@rpnrc))
end

describe Settings do
  context 'when working with an empty settings file,' do
    before(:each) do
      @settings = Settings.new empty_settings_file()
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

  context 'when working with a settings file with an empty hash,' do
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

  context 'when working with a non-empty settings file,' do
    before(:each) do
      @settings = Settings.new temp_settings_file({'stack'=>[Number.new("1").to_h, Number.new("0o2").to_h],
                                                   'registers'=>{'a'=>Number.new("0x3").to_h},
                                                   'macros'=>{'f'=>['3','*']},
                                                   'base'=>8,
                                                   'angle'=>'RAD',
                                                   'colors'=>{
                                                     'normal'=>:black,
                                                     'error'=>:red,
                                                     'title'=>:green,
                                                     'register'=>:yellow,
                                                     'help_heading'=>:blue,
                                                     'help'=>:cyan}})
    end
    after(:each) do
      File.delete @rpnrc
    end

    it 'returns the stack' do
      expect(@settings.stack).to eq([Number.new("1"), Number.new("0o2")])
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
      @settings.stack = [Number.new("3"),Number.new("0o4"),Number.new("1.2-3.4i")]
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
        {'stack'=>[{"0"=>3.0},{"8"=>4.0},{"0"=>"1.2-3.4i"}],
         'registers'=>{'z'=>{"16"=>5.0}},
         'macros'=>{'f'=>['4','/']},
         'base'=>16,
         'angle'=>'DEG',
         'colors'=>{
           'normal'=>'a',
           'error'=>'b',
           'title'=>'c',
           'register'=>'d',
           'help_heading'=>'e',
           'help'=>'f'}})
    end
  end
end
