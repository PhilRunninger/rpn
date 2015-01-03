require 'rspec'
require_relative 'processor'

describe Processor do

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

  # Basic Arithmetic
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

  # Error Handling
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

  # Constants
  it 'should know the value of pi' do
    expect(subject.execute('pi')).to be_within(0.00001).of(3.14159)
  end
  it 'should know the value of e' do
    expect(subject.execute('e')).to be_within(0.0000001).of(2.718281828)
  end

  # Bitwise
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

  # Help
  it 'should display a list of all functions' do
    expect{subject.execute('?')}.to_not raise_error
  end

  # Trigonometric
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

  # Powers and Logarithms
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

  #Stack Manipulation
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

  # Rounding
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

  # Registers
  it 'should copy x to a named register location' do
      subject.execute('12 @a')
      expect(subject.stack).to eq([12])
      expect(subject.registers['a']).to eq(12)
  end
  it 'should put the named register location\'s value on the stack' do
    subject.execute('13 @a')
    subject.execute('a')
    expect(subject.stack).to eq([13, 13])
    expect(subject.registers['a']).to eq(13)
  end
  it 'should return the value of x as an answer' do
    expect(subject.execute('14 @a')).to eq(14)
  end
  it 'should clear all registers' do
    subject.execute('13 @a')
    expect(subject.registers['a']).to eq(13)
    subject.execute('cr')
    expect(subject.registers['a']).to be_nil
  end
end
