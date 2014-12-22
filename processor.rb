require_relative 'ansi_colors'

OPERATORS = [{'type'=>'Basic Arithmetic',
              'operators'=>{'+'=>'Addition', '-'=>'Subtraction', '*'=>'Multiplication', '/'=>'Division', 'div'=>'Integer portion of division', '%'=>'Modulus', '**'=>'Exponentiation'},
              'function'=>'float_2_operator'},
             {'type'=>'Basic Arithmetic',
              'operators'=>{'abs'=>'Absolute value of x'},
              'function' => 'float_1_operator'},
             {'type'=>'Basic Arithmetic',
              'operators'=>{'chs'=>'Change the sign of x'},
              'function' => 'custom_operator'},

             {'type'=>'Rounding',
              'operators'=>{'round'=>'Round to nearest integer', 'truncate'=>'Truncate to integer', 'floor'=>'Round down to nearest integer', 'ceil'=>'Round up to nearest integer'},
              'function'=>'float_1_operator'},

             {'type'=>'Constants',
              'operators'=>{'pi'=>'3.141592653....', 'e'=>'2.718281828...'},
              'function'=>'Math_constant'},

             {'type'=>'Powers and Logarithms',
              'operators'=>{'sqrt'=>'Square Root', 'exp'=>'Raise e to the x power', 'log'=>'Natural Log of x', 'log10'=>'Log (base 10) of x', 'log2'=>'Log (base 2) of x'},
              'function'=>'Math_1_operator'},
             {'type'=>'Powers and Logarithms',
              'operators'=>{'\\'=>'Reciprocal'},
              'function'=>'custom_operator'},

             {'type' => 'Trigonometric',
              'operators' => {'rad'=>'Convert degrees to radians', 'deg'=>'Convert radians to degrees'},
              'function' => 'custom_operator'},

             {'type' => 'Trigonometric',
              'operators' => {'sin'=>'Sine of x in radians', 'cos'=>'Cosine of x in radians', 'tan'=>'Tangent of x in radians', 'asin'=>'Arcsine in radians of x', 'acos'=>'Arccosine in radians of x', 'atan'=>'Artangent in radians of x'},
              'function' => 'Math_1_operator'},

             {'type'=>'Bitwise',
              'operators'=>{'&'=>'AND', '|'=>'OR', '^'=>'XOR', '<<'=>'Left Shift', '>>'=>'Right Shift'},
              'function'=>'int_2_operator'},
             {'type'=>'Bitwise',
              'operators'=>{'~'=>"1's complement"},
              'function'=>'int_1_operator'},

             {'type'=>'Stack Manipulation',
              'operators'=>{'copy'=>'Copy top value on stack', 'del'=>'Delete top value from stack', 'clear'=>'Clear the stack', 'xy'=>'Swap x and y'},
              'function'=>'custom_operator'},

             {'type'=>'Help',
              'operators'=>{'?'=>'Display this list', 'help'=>'More detailed help about the calculator.'},
              'function'=>'help'}]

class Processor
  attr_reader :stack

  def initialize
    @stack = []
  end

  def execute(text)
    save_stack = @stack.dup
    begin
      text.split(' ').each do |value|
        if !(value =~ /^-?\d+(\.\d*)?(e[+-]?\d+)?$/).nil?
          @stack.push value.to_f
        else
          op_info = OPERATORS.select{|item| item['operators'].keys.include?(value)}[0]
          send(op_info['function'], value) if !op_info.nil?
          raise NotImplementedError, "#{value} is not a legal operator" if op_info.nil?
        end
      end
    rescue Exception => exception
      @stack = save_stack.dup
      raise
    end
    @stack.last
  end

  def float_1_operator operator
    x = @stack.pop
    @stack.push x.send(operator.to_sym)
  end
  def float_2_operator operator
    x = @stack.pop
    y = @stack.pop
    @stack.push y.send(operator.to_sym, x)
  end

  def int_1_operator operator
    x = @stack.pop.to_i
    @stack.push x.send(operator.to_sym)
  end
  def int_2_operator operator
    x = @stack.pop.to_i
    y = @stack.pop.to_i
    @stack.push y.send(operator.to_sym, x)
  end

  def Math_constant value
    @stack.push eval("Math::#{value.upcase}")
  end

  def Math_1_operator operator
    x = @stack.pop
    @stack.push eval("Math.#{operator}(#{x})")
  end

  def custom_operator operator
    case operator
      when 'rad'
        @stack.push @stack.pop * Math::PI / 180.0
      when 'deg'
        @stack.push @stack.pop * 180.0 / Math::PI
      when 'chs'
        @stack.push -@stack.pop
      when '\\'
        @stack.push 1/@stack.pop
      when 'copy'
        @stack.push @stack.last
      when 'del'
        @stack.pop
      when 'clear'
        @stack = []
      when 'xy'
        x = @stack.pop
        y = @stack.pop
        @stack.push x
        @stack.push y
    end
  end

  def help value
    if value == '?'
      types = OPERATORS.each_with_index.map{|value,i| [value['type'],i]}.uniq{|s| s.first}.map{|item| item[0]}.each{|group| group}
      puts "#{CYAN_TEXT}-------------------------------------------------------------------------------"
      types.each{ |type|
        puts "#{BLUE_TEXT}#{type}#{GRAY_TEXT}"
        operators = OPERATORS.select{|item| item['type'] == type}.map{|item| item['operators']}
        operators.each{|list| list.each{|operator,value| puts " #{'%8s' % operator}  #{value}"}}
      }
      puts "#{CYAN_TEXT}-------------------------------------------------------------------------------#{RESET_COLORS}"
    else
      puts "#{RED_TEXT}Detailed Help is not written yet.#{RESET_COLORS}"
    end
  end
end
