class Number
  attr_accessor :value, :base_as_entered

  def initialize value, base_as_entered=nil
    if !base_as_entered.nil?
      @value = value
      @base_as_entered = base_as_entered
    elsif value.is_a?(Numeric)
      @value = value.to_f
      @base_as_entered = 0
    else
      complex_test = value.match(/^([+-]?(0b[01]+|0x[0-9A-Fa-f]+|0o[0-7]+|(0|(0?\.\d+)|[1-9]\d*(\.\d+)?)([Ee][+-]?\d+)?))([+-](0b[01]+|0x[0-9A-Fa-f]+|0o[0-7]+|(0|(0?\.\d+)|[1-9]\d*(\.\d+)?)([Ee][+-]?\d+)?))i/)
      if !complex_test.nil?
        real = Number.new(complex_test.captures[0])
        imaginary = Number.new(complex_test.captures[6])
        @value = Complex(real.value, imaginary.value)
        @base_as_entered = 0
      elsif !(value =~ /^[+-]?0b[01]+$/).nil?
        @value = Integer(value).to_f
        @base_as_entered = 2
      elsif !(value =~ /^[+-]?0x[0-9A-Fa-f]+$/).nil?
        @value = Integer(value).to_f
        @base_as_entered = 16
      elsif !(value =~ /^[+-]?0o[0-7]+$/).nil?
        @value = Integer(value).to_f
        @base_as_entered = 8
      elsif !(value =~ /^[+-]?(0|(0?\.\d+)|[1-9]\d*(\.\d+)?)(e[+-]?\d+)?$/i).nil?
        @value = value.to_f
        @base_as_entered = 0
      else
        raise ArgumentError, "#{value} is not a parsable number."
      end
    end
  end

  def ==(other)
    return false if other.nil?
    return @value == other.value && @base_as_entered == other.base_as_entered
  end

  def format base
    return "" if @value.nil?
    if @value.is_a?(Complex)
      real = Number.new(@value.real).format(base)
      imaginary = Number.new(@value.imaginary).format(base)
      return real + (imaginary.start_with?('-') ? imaginary : '+' + imaginary) + 'i'
    else
      base = @base_as_entered if base == 0
      if base > 0
        prefix = (value < 0) ? "-" : ""
        prefix += "0x" if base == 16
        prefix += "0b" if base == 2
        prefix += "0o" if base == 8
        return "#{prefix}#{@value.abs.round.to_s(base)}"
      else
        return @value.round.to_s if @value % 1 == 0
        return @value.to_s unless @value % 1 == 0
      end
    end
  end

  def to_h
    {@base_as_entered.to_s =>  @value}
  end

  def self.from_h data
    self.new data.values[0], data.keys[0].to_i
  end

end
