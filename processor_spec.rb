require 'rspec'
require_relative 'processor'

describe Processor do

    # Basic stack pushing and popping {{{1
    it 'parses a string with one number into a stack with one number' do
        subject.execute '123'
        expect(subject.stack).to eq([123])
    end
    it 'adds to the stack with subsequent calls to execute' do
        subject.execute '42'
        subject.execute '73'
        expect(subject.stack).to eq([42, 73])
    end
    it 'parses a string of two numbers into a stack with two numbers' do
        subject.execute '1.5 32'
        expect(subject.stack).to eq([1.5, 32])
    end
    it 'returns an answer' do
        expect(subject.execute ('2 3 4 5')).to eq(5)
    end

    # Internal utility functions {{{1
    it 'parses a number' do
        expect(subject.parse_number('23')).to eq(23.0)
        expect(subject.parse_number('2.3')).to eq(2.3)
        expect(subject.parse_number('-42')).to eq(-42)
        expect(subject.parse_number('-.25')).to eq(-0.25)
        expect(subject.parse_number('3.14e4')).to eq(31400.0)
        expect(subject.parse_number('5e-3')).to eq(0.005)
        expect(subject.parse_number('4.2.3')).to be_nil
        expect(subject.parse_number('-')).to be_nil
        expect(subject.parse_number('foobar==')).to be_nil
    end
    it 'parses binary numbers' do
        subject.base = 2
        expect(subject.parse_number('101')).to eq(5)
        expect(subject.parse_number('123')).to be_nil
        expect(subject.parse_number('1.0')).to be_nil
        expect(subject.parse_number('-10')).to be_nil
    end
    it 'parses octal numbers' do
        subject.base = 8
        expect(subject.parse_number('37')).to eq(31)
        expect(subject.parse_number('129')).to be_nil
        expect(subject.parse_number('1.2')).to be_nil
        expect(subject.parse_number('-12')).to be_nil
    end
    it 'parses decimal numbers' do
        subject.base = 10
        expect(subject.parse_number('153')).to eq(153)
        expect(subject.parse_number('12a')).to be_nil
        expect(subject.parse_number('1.2')).to be_nil
        expect(subject.parse_number('-12')).to be_nil
    end
    it 'parses hexadecimal numbers' do
        subject.base = 16
        expect(subject.parse_number('1a')).to eq(26)
        expect(subject.parse_number('1A')).to eq(26)
        expect(subject.parse_number('12x')).to be_nil
        expect(subject.parse_number('1.2')).to be_nil
        expect(subject.parse_number('-12')).to be_nil
    end
    it 'parses operators' do
        expect(subject.parse_operator('+')).to be_kind_of(Hash)
        expect(subject.parse_operator('123')).to be_nil
        expect(subject.parse_operator('fubar=')).to be_nil
        expect(subject.parse_operator('=fubar')).to be_nil
    end
    it 'parses registers' do
        expect(subject.parse_register('123')).to be_nil
        expect(subject.parse_register('**')).to be_nil
        expect(subject.parse_register('hot!=')).to be_nil
        expect(subject.parse_register('a1a=')).to be_kind_of(MatchData)
        expect(subject.parse_register('abc==')).to be_kind_of(MatchData)
        expect(subject.parse_register('==def')).to be_kind_of(MatchData)
        expect(subject.parse_register('=ghi')).to be_kind_of(MatchData)
        expect(subject.parse_register('j_k')).to be_kind_of(MatchData)
    end
    it 'will not allow pushing a nonexistent register' do
        subject.execute('12 widgets=')
        expect(subject.parse_register('=widgets')).to be_kind_of(MatchData)
    end
    it 'formats numbers for printing' do
        expect(subject.format([1, 2, 3])).to eq("[1 2 3]")
        expect(subject.format(123)).to eq("123")
        expect(subject.format(-1.23)).to eq("-1.23")
        subject.base = 2
        expect(subject.format(12)).to eq("1100")
        expect(subject.format(-23)).to eq("###")
        expect(subject.format(12.7)).to eq("###")
        subject.base = 8
        expect(subject.format(12)).to eq("14")
        expect(subject.format(-23)).to eq("###")
        expect(subject.format(12.7)).to eq("###")
        subject.base = 10
        expect(subject.format(12)).to eq("12")
        expect(subject.format(-23)).to eq("###")
        expect(subject.format(12.7)).to eq("###")
        subject.base = 16
        expect(subject.format(12)).to eq("c")
        expect(subject.format(-23)).to eq("###")
        expect(subject.format(12.7)).to eq("###")
    end

    # Basic Arithmetic {{{1
    it 'adds two numbers' do
        expect(subject.execute ('1 2 +')).to eq(3)
    end
    it 'subtracts two numbers' do
        expect(subject.execute ('1 2 -')).to eq(-1)
    end
    it 'multiplies two numbers' do
        expect(subject.execute ('3.14 2 *')).to eq(6.28)
    end
    it 'divides two numbers' do
        expect(subject.execute ('1 2 /')).to eq(0.5)
    end
    it 'returns the integer part of division' do
        expect(subject.execute('45.4 7 div')).to eq(6)
    end
    it 'finds the modulus of a number' do
        expect(subject.execute ('23 4 %')).to eq(3)
    end
    it 'raises a number to a power' do
        expect(subject.execute ('2 5 **')).to eq(32)
    end
    it 'changes the sign of the top number' do
        expect(subject.execute('12 chs')).to eq(-12)
    end
    it 'calculates the absolute value of a number' do
        expect(subject.execute('-5 abs')).to eq(5)
        expect(subject.execute('3.14 abs')).to eq(3.14)
    end

    # Error Handling {{{1
    it 'raises an error if not enough operands' do
        expect {subject.execute ('2 +')}.to raise_error
    end
    it 'raises an error if given an unknown operator/register' do
        expect {subject.execute('1 2 snafu')}.to raise_error
        expect {subject.execute('1 2 =foobar')}.to raise_error
    end
    it 'restores the stack to what it was before an exception was raised' do
        subject.execute('42')
        expect {subject.execute('+')}.to raise_error
        expect(subject.stack).to eq([42])
    end

    # Constants {{{1
    it 'knows the value of pi' do
        expect(subject.execute('pi')).to be_within(0.00001).of(3.14159)
    end
    it 'knows the value of e' do
        expect(subject.execute('e')).to be_within(0.0000001).of(2.718281828)
    end

    # Bitwise {{{1
    it 'does bitwise AND' do
        expect(subject.execute('60 13 &')).to eq(12)
    end
    it 'does bitwise OR' do
        expect(subject.execute('60 13 |')).to eq(61)
    end
    it 'does bitwise XOR' do
        expect(subject.execute('60 13 ^')).to eq(49)
    end
    it 'does ones complement' do
        expect(subject.execute('60 ~')).to eq(-61)
    end
    it 'does left shift' do
        expect(subject.execute('60 2 <<')).to eq(240)
    end
    it 'does right shift' do
        expect(subject.execute('60 2 >>')).to eq(15)
    end

    ## Help {{{1
    it 'displays a list of all functions' do
        expect{subject.execute('?')}.to_not raise_error
    end
    it 'launches a web page of RPN tutorials' do
        expect{subject.execute('??')}.to_not raise_error
    end

    # Trigonometric {{{1
    it 'calculates sin of a number in radians' do
        expect(subject.execute('0.5235987755982988 sin')).to be_within(0.000001).of(0.5)
    end
    it 'calculates cos of a number in radians' do
        expect(subject.execute('1.0471975511965976 cos')).to be_within(0.000001).of(0.5)
    end
    it 'calculates tan of a number in radians' do
        expect(subject.execute('0.7853981633974483 tan')).to be_within(0.000001).of(1.0)
    end
    it 'calculates asin of a number in radians' do
        expect(subject.execute('0.5 asin')).to be_within(0.000001).of(0.5235987755982988)
    end
    it 'calculates acos of a number in radians' do
        expect(subject.execute('0.5 acos')).to be_within(0.000001).of(1.0471975511965976)
    end
    it 'calculates atan of a number in radians' do
        expect(subject.execute('1 atan')).to be_within(0.000001).of(0.7853981633974483)
    end

    # Powers and Logarithms {{{1
    it 'calculates the square root of a number' do
        expect(subject.execute('64 sqrt')).to eq(8)
    end
    it 'calculates the reciprocal of a number' do
        expect(subject.execute('4 \\')).to eq(0.25)
    end
    it 'calculates e^x' do
        expect(subject.execute('10 exp')).to be_within(0.000001).of(22026.4657948)
    end
    it 'calculates the natural log of x' do
        expect(subject.execute('99 log')).to be_within(0.000001).of(4.59511985014)
    end
    it 'calculates the log (base 10) of x' do
        expect(subject.execute('99 log10')).to be_within(0.000001).of(1.9956351946)
    end
    it 'calculates the log (base 2) of x' do
        expect(subject.execute('99 log2')).to be_within(0.000001).of(6.62935662008)
    end

    # Stack Manipulation {{{1
    it 'copies the top value on the stack' do
        subject.execute('5 copy')
        expect(subject.stack).to eq([5,5])
    end
    it 'deletes the top value from the stack' do
        subject.execute('5 3 del')
        expect(subject.stack).to eq([5])
    end
    it 'clears the stack completely' do
        subject.execute('1 2 3 4')
        expect(subject.stack).to eq([1,2,3,4])
        subject.execute('cs')
        expect(subject.stack).to eq([])
    end
    it 'exchanges the values in X and Y' do
        subject.execute('1 3 4 xy')
        expect(subject.stack).to eq([1,4,3])
    end

    # Rounding {{{1
    it 'rounds to the nearest integer' do
        expect(subject.execute('3.4 round')).to eq(3)
        expect(subject.execute('4.5 round')).to eq(5)
        expect(subject.execute('7.8 round')).to eq(8)
        expect(subject.execute('-4.2 round')).to eq(-4)
        expect(subject.execute('-4.5 round')).to eq(-5)
        expect(subject.execute('-5.6 round')).to eq(-6)
    end
    it 'rounds down to the nearest integer' do
        expect(subject.execute('3.4 floor')).to eq(3)
        expect(subject.execute('4.5 floor')).to eq(4)
        expect(subject.execute('7.8 floor')).to eq(7)
        expect(subject.execute('-4.2 floor')).to eq(-5)
        expect(subject.execute('-4.5 floor')).to eq(-5)
        expect(subject.execute('-5.6 floor')).to eq(-6)
    end
    it 'rounds up to the nearest integer' do
        expect(subject.execute('3.4 ceil')).to eq(4)
        expect(subject.execute('4.5 ceil')).to eq(5)
        expect(subject.execute('7.8 ceil')).to eq(8)
        expect(subject.execute('-4.2 ceil')).to eq(-4)
        expect(subject.execute('-4.5 ceil')).to eq(-4)
        expect(subject.execute('-5.6 ceil')).to eq(-5)
    end
    it 'truncates to the nearest integer' do
        expect(subject.execute('3.4 truncate')).to eq(3)
        expect(subject.execute('4.5 truncate')).to eq(4)
        expect(subject.execute('7.8 truncate')).to eq(7)
        expect(subject.execute('-4.2 truncate')).to eq(-4)
        expect(subject.execute('-4.5 truncate')).to eq(-4)
        expect(subject.execute('-5.6 truncate')).to eq(-5)
    end

    # Registers {{{1
    it 'copies x to a named register location' do
        subject.execute('12 a=')
        expect(subject.stack).to eq([12])
        expect(subject.registers['a']).to eq(12)
    end
    it 'copies the entire stack to an array value in the named register' do
        subject.execute('4 3 2 1 a==')
        expect(subject.stack).to eq([4,3,2,1])
        expect(subject.registers['a']).to eq([4,3,2,1])
    end
    it 'puts the named register location\'s value on the stack' do
        subject.execute('13 a=')
        subject.execute('=a')
        expect(subject.stack).to eq([13, 13])
        expect(subject.registers['a']).to eq(13)
    end
    it 'puts the named register location\'s value on the stack without equal sign' do
        subject.execute('13 a=')
        subject.execute('a')
        expect(subject.stack).to eq([13, 13])
        expect(subject.registers['a']).to eq(13)
    end
    it 'replaces the stack with the named register location\'s value' do
        subject.execute('12 11 13 a=')
        subject.execute('==a')
        expect(subject.stack).to eq([13])
    end
    it 'puts the values of an array stored in the register onto the stack' do
        subject.registers['sample'] = [1,2,3]
        subject.execute('=sample')
        expect(subject.stack).to eq([1,2,3])
    end
    it 'returns the value of x as an answer' do
        expect(subject.execute('14 a=')).to eq(14)
    end
    it 'clears all registers' do
        subject.execute('13 a=')
        expect(subject.registers['a']).to eq(13)
        subject.execute('cr')
        expect(subject.registers['a']).to be_nil
    end
    it 'will not allow the use of an operator for a register name' do
        expect {subject.execute('5 pi=')}.to raise_error
    end
    it 'throws an exception when nothing to put into register' do
        expect {subject.execute('foo=')}.to raise_error
    end
    it 'throws an exception when register is not defined' do
        expect {subject.execute('foo')}.to raise_error
    end

    # Statistics {{{1
    it 'calculates the factorial of x' do
        expect(subject.execute('0 !')).to eq(1)
        expect(subject.execute('6 !')).to eq(720)
        expect(subject.execute('3.14 !')).to eq(6)
        expect {subject.execute('-5 !')}.to raise_error
    end
    it 'calculates permutation' do
        expect(subject.execute('5 3 perm')).to eq(60)
    end
    it 'calculates combination' do
        expect(subject.execute('5 3 comb')).to eq(10)
    end
    it 'calculates the product of all numbers on the stack' do
        expect(subject.execute('5 4 2 product')).to eq(40)
        expect(subject.registers['sample']).to eq([5,4,2])
    end
    it 'calculates the sum of the stack' do
        expect(subject.execute('1 2 5 sum')).to eq(8)
        expect(subject.registers['sample']).to eq([1,2,5])
    end
    it 'calculates the mean of the stack' do
        expect(subject.execute('2 5 7 11 mean')).to eq(6.25)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'calculates the median of the stack' do
        expect(subject.execute('2 5 7 11 median')).to eq(6)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'calculates the standard deviation of the stack' do
        expect(subject.execute('2 5 7 11 std')).to be_within(0.000001).of(3.774917218)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'calculates the count of the stack' do
        expect(subject.execute('2 5 7 11 count')).to eq(4)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end

    # Unit Conversion {{{1
    it 'shows a list of convertible units' do
        expect{subject.execute('units')}.to_not raise_error
    end
    it 'throws an exception for invalid units' do
        expect{subject.execute('1 foobar>snafu')}.to raise_error
    end
    it 'throws an exception for incompatible units' do
        expect{subject.execute('1 in>rad')}.to raise_error
    end
    it 'throws an exception when nothing to convert' do
        expect{subject.execute('mi>km')}.to raise_error
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
                    result = subject.execute("#{from_value} #{from_units}>#{to_units}")
                    fail_message = "[Expected #{from_value} #{from_units}>#{to_units} to equal #{to_value}. Got #{result}.]"
                rescue Exception => e
                    fail_message = "[Expected #{from_value} #{from_units}>#{to_units} to equal #{to_value}. Got \"#{e.message}\".]"
                end
                expect(result).to be_within(tolerance).of(to_value), fail_message
            }
        }
    end

    # }}}

    # Work in alternate bases {{{1
    it 'switches to binary mode' do
        expect(subject.execute('bin 1101')).to eq(13)
    end
    it 'switches to octal mode' do
        expect(subject.execute('oct 1101')).to eq(577)
    end
    it 'switches to decimal mode' do
        expect(subject.execute('dec 1101')).to eq(1101)
    end
    it 'switches to hexadecimal mode' do
        expect(subject.execute('hex 1101')).to eq(4353)
    end
    it 'switches to real mode' do
        expect(subject.execute('real 1101')).to eq(1101)
    end
    it 'adds numbers of different bases' do
        expect(subject.execute('bin 1011 hex c2 oct 31 + +')).to eq(230)
    end
    # }}}
end
# vim:ft=ruby foldmethod=marker sw=4
