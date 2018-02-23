require 'json'
require 'rspec'
require_relative 'processor'
require_relative 'number'

def temp_settings_file(hash)
    @rpnrc = File.join(ENV['TMPDIR'], '.rpnrc')
    File.open(@rpnrc, 'w') {|f| f.write(hash.to_json)}
    @rpnrc
end

def settings_file_hash
    JSON.parse(File.read(@rpnrc))
end

describe Processor do

    # When working with an empty settings file {{{1
    context 'when working with an empty settings file, ' do
        before (:each) do
            @processor = Processor.new temp_settings_file({})
        end
        after (:each) do
            File.delete @rpnrc
        end

        # # Basic stack pushing and popping {{{2
        it 'parses a string with one number into a stack with one number' do
            @processor.execute '123'
            expect(@processor.stack).to eq([Number.new('123')])
        end
        it 'adds to the stack with subsequent calls to execute' do
            @processor.execute '42'
            @processor.execute '73'
            expect(@processor.stack).to eq([Number.new('42'), Number.new('73')])
        end
        it 'parses a string of two numbers into a stack with two numbers' do
            @processor.execute '1.5 32'
            expect(@processor.stack).to eq([Number.new('1.5'), Number.new('32')])
        end
        it 'returns an answer' do
            expect((@processor.execute ('2 3 4 5')).value).to eq(5)
        end

        # Internal utility functions {{{2
        it 'parses a number' do
            expect(@processor.parse_number('23')).to eq(Number.new('23'))
            expect(@processor.parse_number('2.3')).to eq(Number.new('2.3'))
            expect(@processor.parse_number('-42')).to eq(Number.new('-42'))
            expect(@processor.parse_number('-.25')).to eq(Number.new('-0.25'))
            expect(@processor.parse_number('3.14e4')).to eq(Number.new('31400.0'))
            expect(@processor.parse_number('5e-3')).to eq(Number.new('0.005'))
            expect(@processor.parse_number('4.2.3')).to be_nil
            expect(@processor.parse_number('-')).to be_nil
            expect(@processor.parse_number('foobar==')).to be_nil
            expect(@processor.parse_register('func(')).to be_nil
            expect(@processor.parse_register(')')).to be_nil
        end
        it 'parses binary numbers' do
            expect(@processor.parse_number('0b101')).to eq(Number.new('0b101'))
            expect(@processor.parse_number('-0b10')).to eq(Number.new('-0b10'))
        end
        it 'parses octal numbers' do
            expect(@processor.parse_number('0o37')).to eq(Number.new('0o37'))
            expect(@processor.parse_number('-0o12')).to eq(Number.new('-0o12'))
        end
        it 'parses decimal numbers' do
            expect(@processor.parse_number('153')).to eq(Number.new('153'))
            expect(@processor.parse_number('1.2')).to eq(Number.new('1.2'))
            expect(@processor.parse_number('-12')).to eq(Number.new('-12'))
        end
        it 'parses hexadecimal numbers' do
            expect(@processor.parse_number('0x1a')).to eq(Number.new('0x1a'))
            expect(@processor.parse_number('0x1A')).to eq(Number.new('0x1a'))
            expect(@processor.parse_number('-0x12')).to eq(Number.new('-0x12'))
        end
        it 'parses operators' do
            expect(@processor.parse_operator('+')).to be_kind_of(Hash)
            expect(@processor.parse_operator('123')).to be_nil
            expect(@processor.parse_operator('fubar=')).to be_nil
            expect(@processor.parse_operator('=fubar')).to be_nil
            expect(@processor.parse_register('func(')).to be_nil
            expect(@processor.parse_register(')')).to be_nil
        end
        it 'parses registers' do
            expect(@processor.parse_register('123')).to be_nil
            expect(@processor.parse_register('**')).to be_nil
            expect(@processor.parse_register('hot!=')).to be_nil
            expect(@processor.parse_register('func(')).to be_nil
            expect(@processor.parse_register(')')).to be_nil
            expect(@processor.parse_register('cr:a1a')).to be_kind_of(MatchData)
            expect(@processor.parse_register('>a1a')).to be_kind_of(MatchData)
            expect(@processor.parse_register('>>abc')).to be_kind_of(MatchData)
            expect(@processor.parse_register('<<def')).to be_kind_of(MatchData)
            expect(@processor.parse_register('<ghi')).to be_kind_of(MatchData)
        end
        it 'parses macros' do
            expect(@processor.parse_macro('123')).to be_nil
            expect(@processor.parse_macro('**')).to be_nil
            expect(@processor.parse_macro('abc=')).to be_nil
            expect(@processor.parse_macro('a1a(')).to be_kind_of(MatchData)
            expect(@processor.parse_macro(')')).to be_kind_of(MatchData)
            expect(@processor.parse_macro('f()')).to be_kind_of(MatchData)
            expect(@processor.parse_macro('f')).to be_nil

        end

        it 'will not allow pushing a nonexistent register' do
            @processor.execute('12 >widgets')
            expect(@processor.parse_register('<widgets')).to be_kind_of(MatchData)
        end
        it 'formats numbers for printing' do
            expect(@processor.format([Number.new("1"), Number.new("2"), Number.new("3")])).to eq("[1 2 3]")
            expect(@processor.format(Number.new("123"))).to eq("123")
            expect(@processor.format(Number.new("-1.23"))).to eq("-1.23")
            @processor.base = 2
            expect(@processor.format(Number.new("12"))).to eq("0b1100")
            expect(@processor.format(Number.new("-23"))).to eq("-0b10111")
            expect(@processor.format(Number.new("12.7"))).to eq("0b1101")
            @processor.base = 8
            expect(@processor.format(Number.new("12"))).to eq("0o14")
            expect(@processor.format(Number.new("-23"))).to eq("-0o27")
            expect(@processor.format(Number.new("12.7"))).to eq("0o15")
            @processor.base = 10
            expect(@processor.format(Number.new("12"))).to eq("12")
            expect(@processor.format(Number.new("-23"))).to eq("-23")
            expect(@processor.format(Number.new("12.7"))).to eq("13")
            @processor.base = 16
            expect(@processor.format(Number.new("12"))).to eq("0xc")
            expect(@processor.format(Number.new("-23"))).to eq("-0x17")
            expect(@processor.format(Number.new("12.7"))).to eq("0xd")
        end
        #it 'enables the user to select different colors' do
        #   expect{@processor.execute('colors')}.to_not raise_error
        #end

        # Basic Arithmetic {{{2
        it 'adds two numbers' do
            expect((@processor.execute ('1 2 +')).value).to eq(3)
        end
        it 'subtracts two numbers' do
            expect((@processor.execute ('1 2 -')).value).to eq(-1)
        end
        it 'multiplies two numbers' do
            expect((@processor.execute ('3.14 2 *')).value).to eq(6.28)
        end
        it 'divides two numbers' do
            expect((@processor.execute ('1 2 /')).value).to eq(0.5)
        end
        it 'returns the integer part of division' do
            expect((@processor.execute('45.4 7 div')).value).to eq(6)
        end
        it 'finds the modulus of a number' do
            expect((@processor.execute ('23 4 %')).value).to eq(3)
        end
        it 'raises a number to a power' do
            expect((@processor.execute ('2 5 **')).value).to eq(32)
        end
        it 'changes the sign of the top number' do
            expect((@processor.execute('12 chs')).value).to eq(-12)
        end
        it 'calculates the absolute value of a number' do
            expect((@processor.execute('-5 abs')).value).to eq(5)
            expect((@processor.execute('3.14 abs')).value).to eq(3.14)
        end

        # Error Handling {{{2
        it 'raises an error if not enough operands' do
            expect {@processor.execute ('2 +')}.to raise_error(NoMethodError)
        end
        it 'raises an error if given an unknown operator/register' do
            expect {@processor.execute('1 2 =foobar')}.to raise_error(NotImplementedError)
        end
        it 'restores the stack to what it was before an exception was raised' do
            @processor.execute('42')
            expect {@processor.execute('+')}.to raise_error(NoMethodError)
            expect(@processor.stack).to eq([Number.new(42)])
        end

        # Constants {{{2
        it 'knows the value of pi' do
            expect((@processor.execute('pi')).value).to be_within(0.00001).of(3.14159)
        end
        it 'knows the value of e' do
            expect((@processor.execute('e')).value).to be_within(0.0000001).of(2.718281828)
        end
        it 'knows the value of phi' do
            expect((@processor.execute('phi')).value).to be_within(0.0000001).of(1.618033989)
        end


        # Bitwise {{{2
        it 'does bitwise AND' do
            expect((@processor.execute('60 13 &')).value).to eq(12)
        end
        it 'does bitwise OR' do
            expect((@processor.execute('60 13 |')).value).to eq(61)
        end
        it 'does bitwise XOR' do
            expect((@processor.execute('60 13 ^')).value).to eq(49)
        end
        it 'does ones complement' do
            expect((@processor.execute('60 ~')).value).to eq(-61)
        end
        it 'does left shift' do
            expect((@processor.execute('60 2 <<')).value).to eq(240)
        end
        it 'does right shift' do
            expect((@processor.execute('60 2 >>')).value).to eq(15)
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
            expect((@processor.execute('30 sin')).value).to be_within(0.000001).of(0.5)
        end
        it 'calculates cos of a number in degrees' do
            expect((@processor.execute('60 cos')).value).to be_within(0.000001).of(0.5)
        end
        it 'calculates tan of a number in degrees' do
            expect((@processor.execute('45 tan')).value).to be_within(0.000001).of(1.0)
        end
        it 'calculates asin of a number in degrees' do
            expect((@processor.execute('0.5 asin')).value).to be_within(0.000001).of(30)
        end
        it 'calculates acos of a number in degrees' do
            expect((@processor.execute('0.5 acos')).value).to be_within(0.000001).of(60)
        end
        it 'calculates atan of a number in degrees' do
            expect((@processor.execute('1 atan')).value).to be_within(0.000001).of(45)
        end

        # Powers and Logarithms {{{2
        it 'calculates the square root of a number' do
            expect((@processor.execute('64 sqrt')).value).to eq(8)
        end
        it 'calculates the reciprocal of a number' do
            expect((@processor.execute('4 \\')).value).to eq(0.25)
        end
        it 'calculates e^x' do
            expect((@processor.execute('10 exp')).value).to be_within(0.000001).of(22026.4657948)
        end
        it 'calculates the natural log of x' do
            expect((@processor.execute('99 log')).value).to be_within(0.000001).of(4.59511985014)
        end
        it 'calculates the log (base 10) of x' do
            expect((@processor.execute('99 log10')).value).to be_within(0.000001).of(1.9956351946)
        end
        it 'calculates the log (base 2) of x' do
            expect((@processor.execute('99 log2')).value).to be_within(0.000001).of(6.62935662008)
        end

        # Stack Manipulation {{{2
        it 'copies the top value on the stack' do
            @processor.execute('5 copy')
            expect(@processor.stack).to eq([Number.new(5),Number.new(5)])
        end
        it 'deletes the top value from the stack' do
            @processor.execute('5 3 del')
            expect(@processor.stack).to eq([Number.new(5)])
        end
        it 'clears the stack completely' do
            @processor.execute('1 2 3')
            expect(@processor.stack).to eq([Number.new(1),Number.new(2),Number.new(3)])
            @processor.execute('cs')
            expect(@processor.stack).to eq([])
        end
        it 'exchanges the values in X and Y' do
            @processor.execute('1 3 4 xy')
            expect(@processor.stack).to eq([Number.new(1),Number.new(4),Number.new(3)])
        end

        # Rounding {{{2
        it 'rounds to the nearest integer' do
            expect((@processor.execute('3.4 round')).value).to eq(3)
            expect((@processor.execute('4.5 round')).value).to eq(5)
            expect((@processor.execute('7.8 round')).value).to eq(8)
            expect((@processor.execute('-4.2 round')).value).to eq(-4)
            expect((@processor.execute('-4.5 round')).value).to eq(-5)
            expect((@processor.execute('-5.6 round')).value).to eq(-6)
        end
        it 'rounds down to the nearest integer' do
            expect((@processor.execute('3.4 floor')).value).to eq(3)
            expect((@processor.execute('4.5 floor')).value).to eq(4)
            expect((@processor.execute('7.8 floor')).value).to eq(7)
            expect((@processor.execute('-4.2 floor')).value).to eq(-5)
            expect((@processor.execute('-4.5 floor')).value).to eq(-5)
            expect((@processor.execute('-5.6 floor')).value).to eq(-6)
        end
        it 'rounds up to the nearest integer' do
            expect((@processor.execute('3.4 ceil')).value).to eq(4)
            expect((@processor.execute('4.5 ceil')).value).to eq(5)
            expect((@processor.execute('7.8 ceil')).value).to eq(8)
            expect((@processor.execute('-4.2 ceil')).value).to eq(-4)
            expect((@processor.execute('-4.5 ceil')).value).to eq(-4)
            expect((@processor.execute('-5.6 ceil')).value).to eq(-5)
        end
        it 'truncates to the nearest integer' do
            expect((@processor.execute('3.4 truncate')).value).to eq(3)
            expect((@processor.execute('4.5 truncate')).value).to eq(4)
            expect((@processor.execute('7.8 truncate')).value).to eq(7)
            expect((@processor.execute('-4.2 truncate')).value).to eq(-4)
            expect((@processor.execute('-4.5 truncate')).value).to eq(-4)
            expect((@processor.execute('-5.6 truncate')).value).to eq(-5)
        end

        # Registers {{{2
        it 'copies x to a named register location' do
            @processor.execute('12 >a')
            expect(@processor.stack).to eq([Number.new(12)])
            expect(@processor.registers['a']).to eq(Number.new(12))
        end
        it 'copies the entire stack to an array value in the named register' do
            @processor.execute('4 3 2 1 >>a')
            expect(@processor.stack).to eq([Number.new(4),Number.new(3),Number.new(2),Number.new(1)])
            expect(@processor.registers['a']).to eq([Number.new(4),Number.new(3),Number.new(2),Number.new(1)])
        end
        it 'puts the named register location\'s value on the stack' do
            @processor.execute('13 >a')
            @processor.execute('<a')
            expect(@processor.stack).to eq([Number.new(13), Number.new(13)])
            expect(@processor.registers['a']).to eq(Number.new(13))
        end
        it 'replaces the stack with the named register location\'s value' do
            @processor.execute('12 11 13 >a')
            @processor.execute('<<a')
            expect(@processor.stack).to eq([Number.new(13)])
        end
        it 'puts the values of an array stored in the register onto the stack' do
            @processor.registers['sample'] = [Number.new(1),Number.new(2),Number.new(3)]
            @processor.execute('<sample')
            expect(@processor.stack).to eq([Number.new(1),Number.new(2),Number.new(3)])
        end
        it 'returns the value of x as an answer' do
            expect((@processor.execute('14 >a')).value).to eq(14)
        end
        it 'clears all registers' do
            @processor.execute('13 >a')
            expect(@processor.registers['a']).to eq(Number.new(13))
            @processor.execute('cr')
            expect(@processor.registers['a']).to be_nil
        end
        it 'clears a single register' do
            @processor.execute('13 >a >b')
            expect(@processor.registers['a']).to eq(Number.new(13))
            expect(@processor.registers['b']).to eq(Number.new(13))
            @processor.execute('cr:a')
            expect(@processor.registers['a']).to be_nil
            expect(@processor.registers['b']).to eq(Number.new(13))
        end
        it 'will not allow the use of an operator for a register name' do
            expect {@processor.execute('5 pi=')}.to raise_error(NotImplementedError)
        end
        it 'will not allow the use of a macro for a register name' do
            expect {@processor.execute('f( 3 * ) 5 >f')}.to raise_error(ArgumentError)
        end
        it 'throws an exception when nothing to put into register' do
            expect {@processor.execute('>foo')}.to raise_error(ArgumentError)
        end
        it 'throws an exception when register is not defined' do
            expect {@processor.execute('<foo')}.to raise_error(ArgumentError)
        end

        # Statistics {{{2
        it 'calculates the factorial of x' do
            expect((@processor.execute('0 !')).value).to eq(1)
            expect((@processor.execute('6 !')).value).to eq(720)
            expect((@processor.execute('3.14 !')).value).to eq(6)
            expect {@processor.execute('-5 !')}.to raise_error(RangeError)
        end
        it 'calculates permutation' do
            expect((@processor.execute('5 3 perm')).value).to eq(60)
        end
        it 'calculates combination' do
            expect((@processor.execute('5 3 comb')).value).to eq(10)
        end
        it 'calculates the product of all numbers on the stack' do
            expect((@processor.execute('5 4 2 product')).value).to eq(40)
            expect(@processor.registers['sample']).to eq([Number.new(5),Number.new(4),Number.new(2)])
        end
        it 'calculates the sum of the stack' do
            expect((@processor.execute('1 2 5 sum')).value).to eq(8)
            expect(@processor.registers['sample']).to eq([Number.new(1),Number.new(2),Number.new(5)])
        end
        it 'calculates the mean of the stack' do
            expect((@processor.execute('2 5 7 11 mean')).value).to eq(6.25)
            expect(@processor.registers['sample']).to eq([Number.new(2), Number.new(5), Number.new(7), Number.new(11)])
        end
        it 'calculates the median of the stack' do
            expect((@processor.execute('2 5 7 11 median')).value).to eq(6)
            expect(@processor.registers['sample']).to eq([Number.new(2), Number.new(5), Number.new(7), Number.new(11)])
        end
        it 'calculates the standard deviation of the stack' do
            expect((@processor.execute('2 5 7 11 std')).value).to be_within(0.000001).of(3.774917218)
            expect(@processor.registers['sample']).to eq([Number.new(2), Number.new(5), Number.new(7), Number.new(11)])
        end
        it 'calculates the count of the stack' do
            expect((@processor.execute('2 5 7 11 count')).value).to eq(4)
            expect(@processor.registers['sample']).to eq([Number.new(2), Number.new(5), Number.new(7), Number.new(11)])
        end

        # Unit Conversion {{{2
        #it 'shows a list of convertible units' do
        #    expect{@processor.execute('units')}.to_not raise_error
        #end
        it 'throws an exception for invalid units' do
            expect{@processor.execute('1 foobar>snafu')}.to raise_error(ArgumentError)
        end
        it 'throws an exception for incompatible units' do
            expect{@processor.execute('1 in>rad')}.to raise_error(ArgumentError)
        end
        it 'throws an exception when nothing to convert' do
            expect{@processor.execute('mi>km')}.to raise_error(NoMethodError)
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
                    expect(result.value).to be_within(tolerance).of(to_value), fail_message
                }
            }
        end

        # Work in different angle modes {{{2
        it 'switches to radians' do
            expect((@processor.execute('rad 1 atan')).value).to be_within(0.00001).of(0.78539816)
        end
        it 'switches to degrees' do
            expect((@processor.execute('60 deg cos')).value).to be_within(0.00001).of(0.5)
        end
        it 'defaults to degrees' do
            expect((@processor.execute('30 sin')).value).to be_within(0.00001).of(0.5)
        end

        # Create and run macros {{{2
        it 'stores a macro in the macros hash' do
            @processor.execute('f( 1 2 3 )')
            expect(@processor.macros).to eq({'f' => ['1', '2', '3']})
        end
        it 'stores a macro in the middle of an input string' do
            @processor.execute('90 sin f( 1 2 4 ) 3 5')
            expect(@processor.macros).to eq({'f' => ['1', '2', '4']})
        end
        it 'stores a macro that spans multiple input strings' do
            @processor.execute('f( 1 2 4')
            @processor.execute('8 16')
            @processor.execute('32 )')
            expect(@processor.macros).to eq({'f' => ['1', '2', '4', '8', '16', '32']})
        end
        it 'does not affect the stack when storing a macro' do
            @processor.execute('4 f( 1 2 3 ) 5')
            expect(@processor.stack).to eq([Number.new(4), Number.new(5)])
        end
        it 'clears a macro with an empty function declaration' do
            @processor.execute('f( 1 2 3 )')
            expect(@processor.macros).to eq({'f' => ['1', '2', '3']})
            @processor.execute('f( )')
            expect(@processor.macros).to eq({})
        end
        it 'clears a macro with a single operator' do
            @processor.execute('f( 1 2 3 )')
            expect(@processor.macros).to eq({'f' => ['1', '2', '3']})
            @processor.execute('f()')
            expect(@processor.macros).to eq({})
        end
        it 'clears all macros' do
            @processor.execute('f( 3 * ) g( 4 + )')
            expect(@processor.macros).to eq({'f' => ['3', '*'], 'g' => ['4', '+']})
            @processor.execute('cm')
            expect(@processor.macros).to eq({})
        end
        it 'executes a macro' do
            @processor.execute('f( 3 * ) g( 4 + )')
            expect((@processor.execute('1 f')).value).to eq(3)
        end
        it 'executes multiple macros' do
            @processor.execute('f( 3 * ) g( 4 + )')
            expect((@processor.execute('1 f f g g')).value).to eq(17)
        end
        it 'raises an error when using a register as a macro name' do
            @processor.execute('12 >f')
            expect {@processor.execute ('f( 13 * )')}.to raise_error(ArgumentError)
        end
        it 'raises an error when using an operator as a macro name' do
            expect {@processor.execute ('pi( 123 )')}.to raise_error(ArgumentError)
        end
        it 'can define macros that call other macros' do
            @processor.execute('f( 2 * ) g( f 2 ** )')
            expect(@processor.macros).to eq({'f'=>['2', '*'], 'g'=>['f', '2', '**']})
            expect((@processor.execute('3 g')).value).to eq(36)
        end
        it 'raises an error if a macro calls itself' do
            expect {@processor.execute ('f( 10 * f )')}.to raise_error(ArgumentError)
        end


        # Saving settings in the settings file. {{{2
        it 'saves the stack after one execute' do
            @processor.execute '1'
            expect(settings_file_hash['stack']).to eq([Number.new(1).to_h])
        end
        it 'saves the stack after each execute' do
            @processor.execute '1'
            expect(settings_file_hash['stack']).to eq([Number.new(1).to_h])
            @processor.execute '2'
            expect(settings_file_hash['stack']).to eq([Number.new(1).to_h,Number.new(2).to_h])
            @processor.execute '+'
            expect(settings_file_hash['stack']).to eq([Number.new(3).to_h])
        end
        it 'saves the registers after one is defined' do
            @processor.execute '1'
            expect(settings_file_hash['registers']).to eq({})
            @processor.execute '>a'
            expect(settings_file_hash['registers']).to eq({'a'=>Number.new(1).to_h})
        end
        it 'saves the macros after one is defined' do
            @processor.execute '1'
            expect(settings_file_hash['macros']).to eq({})
            @processor.execute 'f( 4 * )'
            expect(settings_file_hash['macros']).to eq({'f'=>['4', '*']})
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
            expect(settings_file_hash['stack']).to eq([Number.new(1).to_h])
            @processor.execute 'cs'
            expect(settings_file_hash['stack']).to eq([])
        end
        it 'removes the registers from settings when cleared' do
            @processor.execute '1 >a'
            expect(settings_file_hash['registers']).to eq({'a'=>Number.new(1).to_h})
            @processor.execute 'cr'
            expect(settings_file_hash['registers']).to eq({})
        end
        it 'removes the macros from settings when cleared' do
            @processor.execute 'f( 3 * )'
            expect(settings_file_hash['macros']).to eq({'f'=>['3','*']})
            @processor.execute 'cm'
            expect(settings_file_hash['macros']).to eq({})
        end
        it 'removes the stack, registers, and macros from the settings and returns to NORM mode when cleared' do
            @processor.execute '1 >a f( 3 * ) hex rad'
            expect(settings_file_hash['stack']).to eq([Number.new(1).to_h])
            expect(settings_file_hash['registers']).to eq({'a'=>Number.new(1).to_h})
            expect(settings_file_hash['macros']).to eq({'f'=>['3','*']})
            expect(settings_file_hash['base']).to eq(16)
            expect(settings_file_hash['angle']).to eq('RAD')
            @processor.execute 'ca'
            expect(settings_file_hash['stack']).to eq([])
            expect(settings_file_hash['registers']).to eq({})
            expect(settings_file_hash['macros']).to eq({})
            expect(settings_file_hash['base']).to eq(0)
            expect(settings_file_hash['angle']).to eq('DEG')
        end


        # Miscellaneous Commands  {{{2
        it 'clears the stack, registers, and macros with one command' do
            @processor.execute '1 >a f( 3 * ) hex rad'
            expect(@processor.stack).to eq([Number.new(1)])
            expect(@processor.registers).to eq({'a'=>Number.new(1)})
            expect(@processor.macros).to eq({'f'=>['3','*']})
            expect(@processor.base).to eq(16)
            expect(@processor.angle).to eq("RAD")
            @processor.execute 'ca'
            expect(@processor.stack).to eq([])
            expect(@processor.registers).to eq({})
            expect(@processor.macros).to eq({})
            expect(@processor.base).to eq(0)
            expect(@processor.angle).to eq("DEG")
        end
    end

    # When using an exising non-empty settings file {{{1
    context 'when using an exsting non-empty settings file,' do
        before (:each) do
            @processor = Processor.new temp_settings_file({'stack'=>[Number.new("1").to_h,Number.new("2").to_h,Number.new("3").to_h],
                                                           'registers'=>{'a'=>Number.new("4").to_h, 'b'=>Number.new("5.6").to_h},
                                                           'macros'=>{'f'=>['3','*'], 'g'=>['4','+']},
                                                           'base'=>2,
                                                           'angle'=>'RAD'})
        end
        after (:each) do
            File.delete @rpnrc
        end

        it 'remembers the stack from the previous session' do
            expect(@processor.stack).to eq([Number.new("1"),Number.new("2"),Number.new("3")])
        end
        it 'remembers the registers from previous session' do
            expect(@processor.registers['a']).to eq(Number.new("4"))
            expect(@processor.registers['b']).to eq(Number.new("5.6"))
        end
        it 'remembers the macros from previous session' do
            expect(@processor.macros['f']).to eq(['3', '*'])
            expect(@processor.macros['g']).to eq(['4', '+'])
        end
        it 'remembers the base used in last session' do
            expect(@processor.radix).to eq('BIN')
        end
        it 'remembers the angle mode used in last session' do
            expect(@processor.angle_mode).to eq('RAD')
        end

        it 'parses a non-macro as a nil' do
            expect(@processor.parse_macro('h')).to be_nil
        end
    end

    # }}}
end
# vim:ft=ruby foldmethod=marker sw=4
