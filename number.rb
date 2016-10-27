class Number
  attr_accessor :value, :base_as_entered

  def initialize value
    if !(value =~ /^-?0b[01]+$/).nil?
      @value = Integer(value)
      @base_as_entered = 2
    elsif !(value =~ /^-?0x[0-9a-fA-F]+$/).nil?
      @value = Integer(value)
      @base_as_entered = 16
    elsif !(value =~ /^-?0[0-7]+$/).nil?
      @value = Integer(value)
      @base_as_entered = 8
    elsif !(value =~ /^-?[1-9]\d*$/).nil?
      @value = Integer(value)
      @base_as_entered = 10
    elsif !(value =~ /^-?((\.\d+)|\d+(\.\d+)?)(e[+-]?\d+)?$/).nil?
      @value = value.to_f
      @base_as_entered = 0
    end
  end

  def format base
    return "" if @value.nil?
    base = @base_as_entered if base == 0
    if base > 0
      prefix = ""
      prefix = "0x" if base == 16
      prefix = "0b" if base == 2
      prefix = "0" if base == 8
      prefix = "-" + prefix if value < 0
      return "#{prefix}#{@value.abs.round.to_s(base)}"
    else
      return @value.round.to_s if @value % 1 == 0
      return @value.to_s unless @value % 1 == 0
    end
  end
end
