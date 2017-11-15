class Number
  attr_accessor :value, :base_as_entered

  def initialize value, base_as_entered = nil
    if !base_as_entered.nil?
      @value = value
      @base_as_entered = base_as_entered
    elsif !(value =~ /^-?0b[01]+$/).nil?
      @value = Integer(value).to_f
      @base_as_entered = 2
    elsif !(value =~ /^-?0x[0-9a-f]+$/i).nil?
      @value = Integer(value).to_f
      @base_as_entered = 16
    elsif !(value =~ /^-?0[0-7]+$/).nil?
      @value = Integer(value).to_f
      @base_as_entered = 8
    elsif !(value =~ /^-?(0|(0?\.\d+)|[1-9]\d*(\.\d+)?)(e[+-]?\d+)?$/i).nil?
      @value = value.to_f
      @base_as_entered = 0
    elsif value.is_a?(Numeric)
      @value = value.to_f
      @base_as_entered = 0
    else
      raise ArgumentError, "#{value} is not a parsable number."
    end
  end

  def ==(other)
    return false if other.nil?
    return @value == other.value && @base_as_entered == other.base_as_entered
  end

  def format base
    return "" if @value.nil?
    base = @base_as_entered if base == 0
    if base > 0
      prefix = (value < 0) ? "-" : ""
      prefix += "0x" if base == 16
      prefix += "0b" if base == 2
      prefix += "0" if base == 8
      return "#{prefix}#{@value.abs.round.to_s(base)}"
    else
      return @value.round.to_s if @value % 1 == 0
      return @value.to_s unless @value % 1 == 0
    end
  end

  def to_h
    {@base_as_entered.to_s =>  @value}
  end

  def self.from_h data
    self.new data.values[0], data.keys[0].to_i
  end

end
