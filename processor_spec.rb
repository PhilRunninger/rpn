require 'json'
require 'rspec'
require_relative 'processor'

def temp_settings_file(hash)
    @rpnrc = File.join(ENV['TMPDIR'], '.rpnrc')
    File.open(@rpnrc, 'w') {|f| f.write(hash.to_json)}
    @rpnrc
end

def settings_file_hash
    JSON.parse(File.read(@rpnrc))
end

describe Processor do

    # Processor works with default settings {{{1
    context 'processor works with default settings' do
        before (:each) do
            @processor = Processor.new temp_settings_file({})
        end
        after (:each) do
            File.delete @rpnrc
        end

        # Basic stack pushing and popping {{{2
        it 'parses a string with one number into a stack with one number' do
            @processor.execute '123'
            expect(@processor.stack).to eq([123])
        end
        it 'adds to the stack with subsequent calls to execute' do
            @processor.execute '42'
            @processor.execute '73'
            expect(@processor.stack).to eq([42, 73])
        end
        it 'parses a string of two numbers into a stack with two numbers' do
            @processor.execute '1.5 32'
            expect(@processor.stack).to eq([1.5, 32])
        end
        it 'returns an answer' do
            expect(@processor.execute ('2 3 4 5')).to eq(5)
        end

        # Internal utility functions {{{2
        it 'parses a number' do
            expect(@processor.parse_number('23')).to eq(23.0)
            expect(@processor.parse_number('2.3')).to eq(2.3)
            expect(@processor.parse_number('-42')).to eq(-42)
            expect(@processor.parse_number('-.25')).to eq(-0.25)
            expect(@processor.parse_number('3.14e4')).to eq(31400.0)
            expect(@processor.parse_number('5e-3')).to eq(0.005)
            expect(@processor.parse_number('4.2.3')).to be_nil
            expect(@processor.parse_number('-')).to be_nil
            expect(@processor.parse_number('foobar==')).to be_nil
        end
        it 'parses binary numbers' do
            @processor.base = 2
            expect(@processor.parse_number('101')).to eq(5)
            expect(@processor.parse_number('123')).to be_nil
            expect(@processor.parse_number('1.0')).to be_nil
            expect(@processor.parse_number('-10')).to be_nil
        end
        it 'parses octal numbers' do
            @processor.base = 8
            expect(@processor.parse_number('37')).to eq(31)
            expect(@processor.parse_number('129')).to be_nil
            expect(@processor.parse_number('1.2')).to be_nil
            expect(@processor.parse_number('-12')).to be_nil
        end
        it 'parses decimal numbers' do
            @processor.base = 10
            expect(@processor.parse_number('153')).to eq(153)
            expect(@processor.parse_number('12a')).to be_nil
            expect(@processor.parse_number('1.2')).to be_nil
            expect(@processor.parse_number('-12')).to be_nil
        end
        it 'parses hexadecimal numbers' do
            @processor.base = 16
            expect(@processor.parse_number('1a')).to eq(26)
            expect(@processor.parse_number('1A')).to eq(26)
            expect(@processor.parse_number('12x')).to be_nil
            expect(@processor.parse_number('1.2')).to be_nil
            expect(@processor.parse_number('-12')).to be_nil
        end
        it 'parses operators' do
            expect(@processor.parse_operator('+')).to be_kind_of(Hash)
            expect(@processor.parse_operator('123')).to be_nil
            expect(@processor.parse_operator('fubar=')).to be_nil
            expect(@processor.parse_operator('=fubar')).to be_nil
        end
        it 'parses registers' do
            expect(@processor.parse_register('123')).to be_nil
            expect(@processor.parse_register('**')).to be_nil
            expect(@processor.parse_register('hot!=')).to be_nil
            expect(@processor.parse_register('a1a=')).to be_kind_of(MatchData)
            expect(@processor.parse_register('abc==')).to be_kind_of(MatchData)
            expect(@processor.parse_register('==def')).to be_kind_of(MatchData)
            expect(@processor.parse_register('=ghi')).to be_kind_of(MatchData)
            expect(@processor.parse_register('j_k')).to be_kind_of(MatchData)
        end
        it 'will not allow pushing a nonexistent register' do
            @processor.execute('12 widgets=')
            expect(@processor.parse_register('=widgets')).to be_kind_of(MatchData)
        end
        it 'formats numbers for printing' do
            expect(@processor.format([1, 2, 3])).to eq("[1 2 3]")
            expect(@processor.format(123)).to eq("123")
            expect(@processor.format(-1.23)).to eq("-1.23")
            @processor.base = 2
            expect(@processor.format(12)).to eq("1100")
            expect(@processor.format(-23)).to eq("###")
            expect(@processor.format(12.7)).to eq("###")
            @processor.base = 8
            expect(@processor.format(12)).to eq("14")
            expect(@processor.format(-23)).to eq("###")
            expect(@processor.format(12.7)).to eq("###")
            @processor.base = 10
            expect(@processor.format(12)).to eq("12")
            expect(@processor.format(-23)).to eq("###")
            expect(@processor.format(12.7)).to eq("###")
            @processor.base = 16
            expect(@processor.format(12)).to eq("c")
            expect(@processor.format(-23)).to eq("###")
            expect(@processor.format(12.7)).to eq("###")
        end
        it 'enables the user to select different colors' do
           expect{@processor.execute('colors')}.to_not raise_error
        end

        # Basic Arithmetic {{{2
        it 'adds two numbers' do
            expect(@processor.execute ('1 2 +')).to eq(3)
        end
        it 'subtracts two numbers' do
            expect(@processor.execute ('1 2 -')).to eq(-1)
        end
        it 'multiplies two numbers' do
            expect(@processor.execute ('3.14 2 *')).to eq(6.28)
        end
        it 'divides two numbers' do
            expect(@processor.execute ('1 2 /')).to eq(0.5)
        end
        it 'returns the integer part of division' do
            expect(@processor.execute('45.4 7 div')).to eq(6)
        end
        it 'finds the modulus of a number' do
            expect(@processor.execute ('23 4 %')).to eq(3)
        end
        it 'raises a number to a power' do
            expect(@processor.execute ('2 5 **')).to eq(32)
        end
        it 'changes the sign of the top number' do
            expect(@processor.execute('12 chs')).to eq(-12)
        end
        it 'calculates the absolute value of a number' do
            expect(@processor.execute('-5 abs')).to eq(5)
            expect(@processor.execute('3.14 abs')).to eq(3.14)
        end

        # Error Handling {{{2
        it 'raises an error if not enough operands' do
            expect {@processor.execute ('2 +')}.to raise_error
        end
        it 'raises an error if given an unknown operator/register' do
            expect {@processor.execute('1 2 snafu')}.to raise_error
            expect {@processor.execute('1 2 =foobar')}.to raise_error
        end
        it 'restores the stack to what it was before an exception was raised' do
            @processor.execute('42')
            expect {@processor.execute('+')}.to raise_error
            expect(@processor.stack).to eq([42])
        end

        # Constants {{{2
        it 'knows the value of pi' do
            expect(@processor.execute('pi')).to be_within(0.00001).of(3.14159)
        end
        it 'knows the value of e' do
            expect(@processor.execute('e')).to be_within(0.0000001).of(2.718281828)
        end

        # Bitwise {{{2
        it 'does bitwise AND' do
            expect(@processor.execute('60 13 &')).to eq(12)
        end
        it 'does bitwise OR' do
            expect(@processor.execute('60 13 |')).to eq(61)
        end
        it 'does bitwise XOR' do
            expect(@processor.execute('60 13 ^')).to eq(49)
        end
        it 'does ones complement' do
            expect(@processor.execute('60 ~')).to eq(-61)
        end
        it 'does left shift' do
            expect(@processor.execute('60 2 <<')).to eq(240)
        end
        it 'does right shift' do
            expect(@processor.execute('60 2 >>')).to eq(15)
        end

        ## Help {{{2
        #it 'displays a list of all functions' do
        #    expect{@processor.execute('?')}.to_not raise_error
        #end
        #it 'launches a web page of RPN tutorials' do
        #    expect{@processor.execute('??')}.to_not raise_error
        #end

        # Trigonometric {{{2
        it 'calculates sin of a number in degrees' do
            expect(@processor.execute('30 sin')).to be_within(0.000001).of(0.5)
        end
        it 'calculates cos of a number in degrees' do
            expect(@processor.execute('60 cos')).to be_within(0.000001).of(0.5)
        end
        it 'calculates tan of a number in degrees' do
            expect(@processor.execute('45 tan')).to be_within(0.000001).of(1.0)
        end
        it 'calculates asin of a number in degrees' do
            expect(@processor.execute('0.5 asin')).to be_within(0.000001).of(30)
        end
        it 'calculates acos of a number in degrees' do
            expect(@processor.execute('0.5 acos')).to be_within(0.000001).of(60)
        end
        it 'calculates atan of a number in degrees' do
            expect(@processor.execute('1 atan')).to be_within(0.000001).of(45)
        end

        # Powers and Logarithms {{{2
        it 'calculates the square root of a number' do
            expect(@processor.execute('64 sqrt')).to eq(8)
        end
        it 'calculates the reciprocal of a number' do
            expect(@processor.execute('4 \\')).to eq(0.25)
        end
        it 'calculates e^x' do
            expect(@processor.execute('10 exp')).to be_within(0.000001).of(22026.4657948)
        end
        it 'calculates the natural log of x' do
            expect(@processor.execute('99 log')).to be_within(0.000001).of(4.59511985014)
        end
        it 'calculates the log (base 10) of x' do
            expect(@processor.execute('99 log10')).to be_within(0.000001).of(1.9956351946)
        end
        it 'calculates the log (base 2) of x' do
            expect(@processor.execute('99 log2')).to be_within(0.000001).of(6.62935662008)
        end

        # Stack Manipulation {{{2
        it 'copies the top value on the stack' do
            @processor.execute('5 copy')
            expect(@processor.stack).to eq([5,5])
        end
        it 'deletes the top value from the stack' do
            @processor.execute('5 3 del')
            expect(@processor.stack).to eq([5])
        end
        it 'clears the stack completely' do
            @processor.execute('1 2 3 4')
            expect(@processor.stack).to eq([1,2,3,4])
            @processor.execute('cs')
            expect(@processor.stack).to eq([])
        end
        it 'exchanges the values in X and Y' do
            @processor.execute('1 3 4 xy')
            expect(@processor.stack).to eq([1,4,3])
        end

        # Rounding {{{2
        it 'rounds to the nearest integer' do
            expect(@processor.execute('3.4 round')).to eq(3)
            expect(@processor.execute('4.5 round')).to eq(5)
            expect(@processor.execute('7.8 round')).to eq(8)
            expect(@processor.execute('-4.2 round')).to eq(-4)
            expect(@processor.execute('-4.5 round')).to eq(-5)
            expect(@processor.execute('-5.6 round')).to eq(-6)
        end
        it 'rounds down to the nearest integer' do
            expect(@processor.execute('3.4 floor')).to eq(3)
            expect(@processor.execute('4.5 floor')).to eq(4)
            expect(@processor.execute('7.8 floor')).to eq(7)
            expect(@processor.execute('-4.2 floor')).to eq(-5)
            expect(@processor.execute('-4.5 floor')).to eq(-5)
            expect(@processor.execute('-5.6 floor')).to eq(-6)
        end
        it 'rounds up to the nearest integer' do
            expect(@processor.execute('3.4 ceil')).to eq(4)
            expect(@processor.execute('4.5 ceil')).to eq(5)
            expect(@processor.execute('7.8 ceil')).to eq(8)
            expect(@processor.execute('-4.2 ceil')).to eq(-4)
            expect(@processor.execute('-4.5 ceil')).to eq(-4)
            expect(@processor.execute('-5.6 ceil')).to eq(-5)
        end
        it 'truncates to the nearest integer' do
            expect(@processor.execute('3.4 truncate')).to eq(3)
            expect(@processor.execute('4.5 truncate')).to eq(4)
            expect(@processor.execute('7.8 truncate')).to eq(7)
            expect(@processor.execute('-4.2 truncate')).to eq(-4)
            expect(@processor.execute('-4.5 truncate')).to eq(-4)
            expect(@processor.execute('-5.6 truncate')).to eq(-5)
        end

        # Registers {{{2
        it 'copies x to a named register location' do
            @processor.execute('12 a=')
            expect(@processor.stack).to eq([12])
            expect(@processor.registers['a']).to eq(12)
        end
        it 'copies the entire stack to an array value in the named register' do
            @processor.execute('4 3 2 1 a==')
            expect(@processor.stack).to eq([4,3,2,1])
            expect(@processor.registers['a']).to eq([4,3,2,1])
        end
        it 'puts the named register location\'s value on the stack' do
            @processor.execute('13 a=')
            @processor.execute('=a')
            expect(@processor.stack).to eq([13, 13])
            expect(@processor.registers['a']).to eq(13)
        end
        it 'puts the named register location\'s value on the stack without equal sign' do
            @processor.execute('13 a=')
            @processor.execute('a')
            expect(@processor.stack).to eq([13, 13])
            expect(@processor.registers['a']).to eq(13)
        end
        it 'replaces the stack with the named register location\'s value' do
            @processor.execute('12 11 13 a=')
            @processor.execute('==a')
            expect(@processor.stack).to eq([13])
        end
        it 'puts the values of an array stored in the register onto the stack' do
            @processor.registers['sample'] = [1,2,3]
            @processor.execute('=sample')
            expect(@processor.stack).to eq([1,2,3])
        end
        it 'returns the value of x as an answer' do
            expect(@processor.execute('14 a=')).to eq(14)
        end
        it 'clears all registers' do
            @processor.execute('13 a=')
            expect(@processor.registers['a']).to eq(13)
            @processor.execute('cr')
            expect(@processor.registers['a']).to be_nil
        end
        it 'will not allow the use of an operator for a register name' do
            expect {@processor.execute('5 pi=')}.to raise_error
        end
        it 'throws an exception when nothing to put into register' do
            expect {@processor.execute('foo=')}.to raise_error
        end
        it 'throws an exception when register is not defined' do
            expect {@processor.execute('foo')}.to raise_error
        end

        # Statistics {{{2
        it 'calculates the factorial of x' do
            expect(@processor.execute('0 !')).to eq(1)
            expect(@processor.execute('6 !')).to eq(720)
            expect(@processor.execute('3.14 !')).to eq(6)
            expect {@processor.execute('-5 !')}.to raise_error
        end
        it 'calculates permutation' do
            expect(@processor.execute('5 3 perm')).to eq(60)
        end
        it 'calculates combination' do
            expect(@processor.execute('5 3 comb')).to eq(10)
        end
        it 'calculates the product of all numbers on the stack' do
            expect(@processor.execute('5 4 2 product')).to eq(40)
            expect(@processor.registers['sample']).to eq([5,4,2])
        end
        it 'calculates the sum of the stack' do
            expect(@processor.execute('1 2 5 sum')).to eq(8)
            expect(@processor.registers['sample']).to eq([1,2,5])
        end
        it 'calculates the mean of the stack' do
            expect(@processor.execute('2 5 7 11 mean')).to eq(6.25)
            expect(@processor.registers['sample']).to eq([2, 5, 7, 11])
        end
        it 'calculates the median of the stack' do
            expect(@processor.execute('2 5 7 11 median')).to eq(6)
            expect(@processor.registers['sample']).to eq([2, 5, 7, 11])
        end
        it 'calculates the standard deviation of the stack' do
            expect(@processor.execute('2 5 7 11 std')).to be_within(0.000001).of(3.774917218)
            expect(@processor.registers['sample']).to eq([2, 5, 7, 11])
        end
        it 'calculates the count of the stack' do
            expect(@processor.execute('2 5 7 11 count')).to eq(4)
            expect(@processor.registers['sample']).to eq([2, 5, 7, 11])
        end

        # Unit Conversion {{{2
        #it 'shows a list of convertible units' do
        #    expect{@processor.execute('units')}.to_not raise_error
        #end
        it 'throws an exception for invalid units' do
            expect{@processor.execute('1 foobar>snafu')}.to raise_error
        end
        it 'throws an exception for incompatible units' do
            expect{@processor.execute('1 in>rad')}.to raise_error
        end
        it 'throws an exception when nothing to convert' do
            expect{@processor.execute('mi>km')}.to raise_error
        end

        it 'converts lengths correctly' do
            test_conversions([{'km'=>1, 'm'=>1e3, 'cm'=>1e5, 'mm'=>1e6, 'micron'=>1e9, 'nm'=>1e12, 'mi'=>0.62137119223733397666365, 'yd'=>1093.6132983377076, 'ft'=>3280.839895013123, 'in'=>39370.07874015748, 'tolerance'=>1e-7}])
        end
        it 'converts speed correctly' do
            test_conversions([{'mi/hr'=>60, 'km/hr'=>96.56064, 'ft/sec'=>88, 'm/sec'=>26.8224, 'ft/hr'=>316800, 'tolerance'=>1e-7}])
        end
        it 'converts time correctly' do
            test_conversions([{'hr' => 1, 'min' => 60, 'sec' => 3600} ])
        end
        it 'converts weights correctly' do
            test_conversions([{'kg'=>1, 'g'=>1000, 'mg'=>1000000, 'N'=>9.80665002864, 'ton'=>0.00110231, 'lb'=>2.20462, 'oz'=>35.27392}])
        end
        it 'converts kitchen measurements correctly' do
            test_conversions([{'gallon'=>1, 'quart'=>4 ,'pint' =>8 ,'cup'  =>16 ,'tbsp' => 256 ,'tsp'  => 768, 'ounce'=> 128} ])
        end
        it 'converts temperatures correctly' do
            test_conversions([{'C'=>0, 'F'=>32, 'K'=>273.15},
                              {'C'=>100, 'F'=>212, 'K'=>373.15}])
        end
        it 'converts angles correctly' do
            test_conversions([{'deg'=>30, 'rad'=>0.5235987755982988, 'tolerance'=>1e-7}])
        end

        def test_conversions(conditions)
            conditions.each{|condition|
                tolerance = condition.delete('tolerance') || 0
                condition.to_a.permutation(2).to_a.each{|trial|
                    print "~"
                    trial = trial.flatten
                    from_units = trial[0]
                    from_value = trial[1]
                    to_units = trial[2]
                    to_value = trial[3]
                    begin
                        result = @processor.execute("#{from_value} #{from_units}>#{to_units}")
                        fail_message = "[Expected #{from_value} #{from_units}>#{to_units} to equal #{to_value}. Got #{result}.]"
                    rescue Exception => e
                        fail_message = "[Expected #{from_value} #{from_units}>#{to_units} to equal #{to_value}. Got \"#{e.message}\".]"
                    end
                    expect(result).to be_within(tolerance).of(to_value), fail_message
                }
            }
        end

        # Work in alternate bases {{{2
        it 'switches to binary mode' do
            expect(@processor.execute('bin 1101')).to eq(13)
        end
        it 'switches to octal mode' do
            expect(@processor.execute('oct 1101')).to eq(577)
        end
        it 'switches to decimal mode' do
            expect(@processor.execute('dec 1101')).to eq(1101)
        end
        it 'switches to hexadecimal mode' do
            expect(@processor.execute('hex 1101')).to eq(4353)
        end
        it 'switches to real mode' do
            expect(@processor.execute('real 1101')).to eq(1101)
        end
        it 'adds numbers of different bases' do
            expect(@processor.execute('bin 1011 hex c2 oct 31 + +')).to eq(230)
        end

        # Work in different angle modes {{{2
        it 'switches to radians' do
            expect(@processor.execute('rad 1 atan')).to be_within(0.00001).of(0.78539816)
        end
        it 'switches to degrees' do
            expect(@processor.execute('60 deg cos')).to be_within(0.00001).of(0.5)
        end
        it 'defaults to degrees' do
            expect(@processor.execute('30 sin')).to be_within(0.00001).of(0.5)
        end

        # }}}
    end

    # Processor initializes using the settings file {{{1
    context 'processor initializes using the settings file' do
        before (:each) do
            @processor = Processor.new temp_settings_file({'stack'=>[1,2,3], 'registers'=>{'a'=>4, 'b'=>5.6}, 'base'=>2, 'angle'=>'RAD'})
        end
        after (:each) do
            File.delete @rpnrc
        end

        it 'remembers the stack from the previous session' do
            expect(@processor.stack).to eq([1,2,3])
        end
        it 'remembers the registers from previous session' do
            expect(@processor.registers['a']).to eq(4)
            expect(@processor.registers['b']).to eq(5.6)
        end
        it 'remembers the base used in last session' do
            expect(@processor.radix).to eq('BIN')
        end
        it 'remembers the angle mode used in last session' do
            expect(@processor.angle_mode).to eq('RAD')
        end
    end

    # Processor saves settings file after execute {{{1
    context 'processor saves settings file after execute' do
        before (:each) do
            @processor = Processor.new temp_settings_file({})
        end
        after (:each) do
            File.delete @rpnrc
        end

        it 'saves the stack after one execute' do
            @processor.execute '1'
            expect(settings_file_hash['stack']).to eq([1])
        end
        it 'saves the stack after each execute' do
            @processor.execute '1'
            expect(settings_file_hash['stack']).to eq([1])
            @processor.execute '2'
            expect(settings_file_hash['stack']).to eq([1,2])
            @processor.execute '+'
            expect(settings_file_hash['stack']).to eq([3])
        end
        it 'saves the registers after one is defined' do
            @processor.execute '1'
            expect(settings_file_hash['registers']).to eq({})
            @processor.execute 'a='
            expect(settings_file_hash['registers']).to eq({'a'=>1})
        end
        it 'saves the base always' do
            @processor.execute '1'
            expect(settings_file_hash['base']).to eq(0)
            @processor.execute 'hex'
            expect(settings_file_hash['base']).to eq(16)
        end
        it 'saves the angle mode always' do
            @processor.execute '1'
            expect(settings_file_hash['angle']).to eq('DEG')
            @processor.execute 'rad'
            expect(settings_file_hash['angle']).to eq('RAD')
        end
        it 'removes the stack from settings when cleared' do
            @processor.execute '1'
            expect(settings_file_hash['stack']).to eq([1])
            @processor.execute 'cs'
            expect(settings_file_hash['stack']).to eq([])
        end
        it 'removes the registers from settings when cleared' do
            @processor.execute '1 a='
            expect(settings_file_hash['registers']).to eq({'a'=>1})
            @processor.execute 'cr'
            expect(settings_file_hash['registers']).to eq({})
        end
    end

    # }}}
end
# vim:ft=ruby foldmethod=marker sw=4
