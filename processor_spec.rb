require 'rspec'
require_relative 'processor'

describe Processor do

    # Basic stack pushing and popping {{{1
    it 'should parse a string with one number into a stack with one number' do
        subject.execute '123'
        expect(subject.stack).to eq([123])
    end
    it 'should add to the stack with subsequent calls to execute' do
        subject.execute '42'
        subject.execute '73'
        expect(subject.stack).to eq([42, 73])
    end
    it 'should parse a string of two numbers into a stack with two numbers' do
        subject.execute '1.5 32'
        expect(subject.stack).to eq([1.5, 32])
    end
    it 'should return an answer' do
        expect(subject.execute ('2 3 4 5')).to eq(5)
    end

    # Internal utility functions {{{1
    it 'should parse a number' do
        expect(subject.parse_number('23')).to eq(23.0)
        expect(subject.parse_number('2.3')).to eq(2.3)
        expect(subject.parse_number('-42')).to eq(-42)
        expect(subject.parse_number('-.25')).to eq(-0.25)
        expect(subject.parse_number('3.14e4')).to eq(31400.0)
        expect(subject.parse_number('5e-3')).to eq(0.005)
        expect(subject.parse_number('4.2.3')).to be_nil
        expect(subject.parse_number('-')).to be_nil
        expect(subject.parse_number('<<foobar')).to be_nil
    end
    it 'should parse operators' do
        expect(subject.parse_operator('+')).to be_kind_of(Hash)
        expect(subject.parse_operator('123')).to be_nil
        expect(subject.parse_operator('<fubar')).to be_nil
        expect(subject.parse_operator('>fubar')).to be_nil
    end
    it 'should parse registers' do
        expect(subject.parse_register('123')).to be_nil
        expect(subject.parse_register('**')).to be_nil
        expect(subject.parse_register('sin')).to be_nil
        expect(subject.parse_register('<hot!')).to be_nil
        expect(subject.parse_register('<a1a')).to be_kind_of(MatchData)
        expect(subject.parse_register('<<abc')).to be_kind_of(MatchData)
    end
    it 'should not allow pushing a nonexistent register' do
        expect(subject.parse_register('>widgets')).to be_nil
        subject.execute('12 <widgets')
        expect(subject.parse_register('>widgets')).to be_kind_of(MatchData)
    end

    # Basic Arithmetic {{{1
    it 'should add two numbers' do
        expect(subject.execute ('1 2 +')).to eq(3)
    end
    it 'should subtract two numbers' do
        expect(subject.execute ('1 2 -')).to eq(-1)
    end
    it 'should multiply two numbers' do
        expect(subject.execute ('3.14 2 *')).to eq(6.28)
    end
    it 'should divide two numbers' do
        expect(subject.execute ('1 2 /')).to eq(0.5)
    end
    it 'should return the integer part of division' do
        expect(subject.execute('45.4 7 div')).to eq(6)
    end
    it 'should find the modulus of a number' do
        expect(subject.execute ('23 4 %')).to eq(3)
    end
    it 'should raise a number to a power' do
        expect(subject.execute ('2 5 **')).to eq(32)
    end
    it 'should change the sign of the top number' do
        expect(subject.execute('12 chs')).to eq(-12)
    end
    it 'should calculate the absolute value of a number' do
        expect(subject.execute('-5 abs')).to eq(5)
        expect(subject.execute('3.14 abs')).to eq(3.14)
    end

    # Error Handling {{{1
    it 'should raise an error if not enough operands' do
        expect {subject.execute ('2 +')}.to raise_error
    end
    it 'should raise an error if given a bad operator' do
        expect {subject.execute('1 2 snafu')}.to raise_error
    end
    it 'should restore the stack to what it was before an exception was raised' do
        subject.execute('42')
        expect {subject.execute('+')}.to raise_error
        expect(subject.stack).to eq([42])
    end

    # Constants {{{1
    it 'should know the value of pi' do
        expect(subject.execute('pi')).to be_within(0.00001).of(3.14159)
    end
    it 'should know the value of e' do
        expect(subject.execute('e')).to be_within(0.0000001).of(2.718281828)
    end

    # Bitwise {{{1
    it 'should do bitwise AND' do
        expect(subject.execute('60 13 &')).to eq(12)
    end
    it 'should do bitwise OR' do
        expect(subject.execute('60 13 |')).to eq(61)
    end
    it 'should do bitwise XOR' do
        expect(subject.execute('60 13 ^')).to eq(49)
    end
    it 'should do ones complement' do
        expect(subject.execute('60 ~')).to eq(-61)
    end
    it 'should do left shift' do
        expect(subject.execute('60 2 <<')).to eq(240)
    end
    it 'should do right shift' do
        expect(subject.execute('60 2 >>')).to eq(15)
    end

    # Help {{{1
    it 'should display a list of all functions' do
        expect{subject.execute('?')}.to_not raise_error
    end

    # Trigonometric {{{1
    it 'should convert degrees to radians' do
        expect(subject.execute('30 rad')).to be_within(0.00000001).of(0.5235987755982988)
    end
    it 'should convert radians to degrees' do
        expect(subject.execute('0.5235987755982988 deg')).to be_within(0.00000001).of(30)
    end
    it 'should calculate sin of a number in radians' do
        expect(subject.execute('30 rad sin')).to be_within(0.000001).of(0.5)
    end
    it 'should calculate cos of a number in radians' do
        expect(subject.execute('60 rad cos')).to be_within(0.000001).of(0.5)
    end
    it 'should calculate tan of a number in radians' do
        expect(subject.execute('45 rad tan')).to be_within(0.000001).of(1.0)
    end
    it 'should calculate asin of a number in radians' do
        expect(subject.execute('0.5 asin deg')).to be_within(0.000001).of(30.0)
    end
    it 'should calculate acos of a number in radians' do
        expect(subject.execute('0.5 acos deg')).to be_within(0.000001).of(60.0)
    end
    it 'should calculate atan of a number in radians' do
        expect(subject.execute('1 atan deg')).to be_within(0.000001).of(45)
    end

    # Powers and Logarithms {{{1
    it 'should calculate the square root of a number' do
        expect(subject.execute('64 sqrt')).to eq(8)
    end
    it 'should calculate the reciprocal of a number' do
        expect(subject.execute('4 \\')).to eq(0.25)
    end
    it 'should calculate e^x' do
        expect(subject.execute('10 exp')).to be_within(0.000001).of(22026.4657948)
    end
    it 'should calculate the natural log of x' do
        expect(subject.execute('99 log')).to be_within(0.000001).of(4.59511985014)
    end
    it 'should calculate the log (base 10) of x' do
        expect(subject.execute('99 log10')).to be_within(0.000001).of(1.9956351946)
    end
    it 'should calculate the log (base 2) of x' do
        expect(subject.execute('99 log2')).to be_within(0.000001).of(6.62935662008)
    end

    # Stack Manipulation {{{1
    it 'should copy the top value on the stack' do
        subject.execute('5 copy')
        expect(subject.stack).to eq([5,5])
    end
    it 'should delete the top value from the stack' do
        subject.execute('5 3 del')
        expect(subject.stack).to eq([5])
    end
    it 'should clear the stack completely' do
        subject.execute('1 2 3 4')
        expect(subject.stack).to eq([1,2,3,4])
        subject.execute('cs')
        expect(subject.stack).to eq([])
    end
    it 'should exchange the values in X and Y' do
        subject.execute('1 3 4 xy')
        expect(subject.stack).to eq([1,4,3])
    end

    # Rounding {{{1
    it 'should round to the nearest integer' do
        expect(subject.execute('3.4 round')).to eq(3)
        expect(subject.execute('4.5 round')).to eq(5)
        expect(subject.execute('7.8 round')).to eq(8)
        expect(subject.execute('-4.2 round')).to eq(-4)
        expect(subject.execute('-4.5 round')).to eq(-5)
        expect(subject.execute('-5.6 round')).to eq(-6)
    end
    it 'should round down to the nearest integer' do
        expect(subject.execute('3.4 floor')).to eq(3)
        expect(subject.execute('4.5 floor')).to eq(4)
        expect(subject.execute('7.8 floor')).to eq(7)
        expect(subject.execute('-4.2 floor')).to eq(-5)
        expect(subject.execute('-4.5 floor')).to eq(-5)
        expect(subject.execute('-5.6 floor')).to eq(-6)
    end
    it 'should round up to the nearest integer' do
        expect(subject.execute('3.4 ceil')).to eq(4)
        expect(subject.execute('4.5 ceil')).to eq(5)
        expect(subject.execute('7.8 ceil')).to eq(8)
        expect(subject.execute('-4.2 ceil')).to eq(-4)
        expect(subject.execute('-4.5 ceil')).to eq(-4)
        expect(subject.execute('-5.6 ceil')).to eq(-5)
    end
    it 'should truncate to the nearest integer' do
        expect(subject.execute('3.4 truncate')).to eq(3)
        expect(subject.execute('4.5 truncate')).to eq(4)
        expect(subject.execute('7.8 truncate')).to eq(7)
        expect(subject.execute('-4.2 truncate')).to eq(-4)
        expect(subject.execute('-4.5 truncate')).to eq(-4)
        expect(subject.execute('-5.6 truncate')).to eq(-5)
    end

    # Registers {{{1
    it 'should copy x to a named register location' do
        subject.execute('12 <a')
        expect(subject.stack).to eq([12])
        expect(subject.registers['a']).to eq(12)
    end
    it 'should copy the entire stack to an array value in the named register' do
        subject.execute('4 3 2 1 <<a')
        expect(subject.stack).to eq([4,3,2,1])
        expect(subject.registers['a']).to eq([4,3,2,1])
    end
    it 'should put the named register location\'s value on the stack' do
        subject.execute('13 <a')
        subject.execute('>a')
        expect(subject.stack).to eq([13, 13])
        expect(subject.registers['a']).to eq(13)
    end
    it 'should replace the stack with the named register location\'s value' do
        subject.execute('12 11 13 <a')
        subject.execute('>>a')
        expect(subject.stack).to eq([13])
    end
    it 'should put the values of an array stored in the register onto the stack' do
        subject.registers['sample'] = [1,2,3]
        subject.execute('>sample')
        expect(subject.stack).to eq([1,2,3])
    end
    it 'should return the value of x as an answer' do
        expect(subject.execute('14 <a')).to eq(14)
    end
    it 'should clear all registers' do
        subject.execute('13 <a')
        expect(subject.registers['a']).to eq(13)
        subject.execute('cr')
        expect(subject.registers['a']).to be_nil
    end
    it 'should not allow the use of an operator for a register name' do
        expect {subject.execute('pi <pi')}.to raise_error
    end
    it 'should throw an exception when nothing to put into register' do
        expect {subject.execute('<foo')}.to raise_error
    end

    # Statistics {{{1
    it 'should calculate the factorial of x' do
        expect(subject.execute('0 !')).to eq(1)
        expect(subject.execute('6 !')).to eq(720)
        expect(subject.execute('3.14 !')).to eq(6)
        expect {subject.execute('-5 !')}.to raise_error
    end
    it 'should calculate permutation' do
        expect(subject.execute('5 3 perm')).to eq(60)
    end
    it 'should calculate combination' do
        expect(subject.execute('5 3 comb')).to eq(10)
    end
    it 'should calculate the product of all numbers on the stack' do
        expect(subject.execute('5 4 2 product')).to eq(40)
        expect(subject.registers['sample']).to eq([5,4,2])
    end
    it 'should calculate the sum of the stack' do
        expect(subject.execute('1 2 5 sum')).to eq(8)
        expect(subject.registers['sample']).to eq([1,2,5])
    end
    it 'should calculate the mean of the stack' do
        expect(subject.execute('2 5 7 11 mean')).to eq(6.25)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'should calculate the median of the stack' do
        expect(subject.execute('2 5 7 11 median')).to eq(6)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'should calculate the standard deviation of the stack' do
        expect(subject.execute('2 5 7 11 std')).to be_within(0.000001).of(3.774917218)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
    it 'should calculate the count of the stack' do
        expect(subject.execute('2 5 7 11 count')).to eq(4)
        expect(subject.registers['sample']).to eq([2, 5, 7, 11])
    end
# }}}
end
# vim:ft=ruby foldmethod=marker sw=4
