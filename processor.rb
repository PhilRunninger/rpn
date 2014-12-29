require 'io/console'
require_relative 'ansi_colors'

OPERATOR_WIDTH = 8
OPERATORS = [{'type'=>'Basic Arithmetic',
              'operators'=>{'+'=>'Addition','-'=>'Subtraction','*'=>'Multiplication','/'=>'Division','div'=>'Integer portion of division','%'=>'Modulus','**'=>'Exponentiation'},
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
              'operators' => {'sin'=>'Sine of x in radians', 'asin'=>'Arcsine in radians of x', 'cos'=>'Cosine of x in radians', 'acos'=>'Arccosine in radians of x', 'tan'=>'Tangent of x in radians', 'atan'=>'Artangent in radians of x'},
              'function' => 'Math_1_operator'},

             {'type'=>'Bitwise',
              'operators'=>{'&'=>'AND', '|'=>'OR', '^'=>'XOR', '<<'=>'Left Shift', '>>'=>'Right Shift'},
              'function'=>'int_2_operator'},
             {'type'=>'Bitwise',
              'operators'=>{'~'=>"1's complement"},
              'function'=>'int_1_operator'},

             {'type'=>'Stack Manipulation',
              'operators'=>{'copy'=>'Copy top value on stack', 'del'=>'Delete top value from stack', 'cs'=>'Clear the stack', 'xy'=>'Swap x and y'},
              'function'=>'custom_operator'},

             {'type'=>'Memory',
              'operators'=>{'cm'=>'Clear memory values'},
              'function'=>'custom_operator'},

             {'type'=>'Help',
              'operators'=>{'?'=>'Display this list'},
              'function'=>'custom_operator'}
            ]

class Processor
  attr_reader :stack, :memories

  def initialize
    @stack = []
    @memories = {}
  end

  def execute(text)
    save_stack = @stack.dup
    begin
      text.split(' ').each do |value|
        number = parse_number(value)
        operator = parse_operator(value)
        register = parse_register(value)

        if !number.nil?
          @stack.push number
        elsif !operator.nil?
          send(operator['function'], value)
        elsif !register.nil?
          memory_operator value.start_with?('@'), register
        else
          raise NotImplementedError, "#{value} is not a valid number, operator, or memory"
        end
      end
    rescue Exception => exception
      @stack = save_stack.dup
      raise
    end
    @stack.delete(nil)
    @stack.last
  end

  def parse_number value
      (value =~ /^-?((\.\d+)|\d+(\.\d+)?)(e[+-]?\d+)?$/).nil? ? nil : value.to_f
  end

  def parse_operator value
      OPERATORS.select{|item| item['operators'].keys.include?(value)}[0]
  end

  def parse_register value
    name = value.match(/^@?([a-z]+\d*)$/i)
    return name if name.nil?
    name = name.captures[0]
    return name if value.start_with?('@') or !@memories[name].nil?
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
      when 'cs'
        @stack = []
      when 'cm'
        @memories = {}
      when 'xy'
        x = @stack.pop
        y = @stack.pop
        @stack.push x
        @stack.push y
      when '?'
        categories = OPERATORS.each_with_index.map{|value,i| [value['type'],i]}.uniq{|s| s.first}.map{|item| item[0]}.each{|group| group}
        puts "#{HIGHLIGHT_COLOR}-------------------------------------------------------------------------------"
        categories.each{ |type|
          puts "#{HELP_CATEGORY}#{type}"
          operators = OPERATORS.select{|item| item['type'] == type}.map{|item| item['operators']}
          operators = operators.inject({}) {|acc, h| acc.merge(h)}
          right_column = false
          operators.each{|operator,description| 
              text = sprintf " #{HIGHLIGHT_COLOR}%#{OPERATOR_WIDTH}s  #{HELP_TEXT}%-#{description_width}s", operator, description
              if text.length - 10 >= console_columns / 2
                  puts "" if right_column
                  puts "#{text}"
                  right_column = false
              else
                  print "#{text}"
                  puts "" if right_column
                  right_column = !right_column
              end
          }
          puts "" if right_column
        }
        puts "#{HIGHLIGHT_COLOR}-------------------------------------------------------------------------------#{RESET_COLORS}"
    end
  end

  def console_columns
      rows, columns = IO.console.winsize
      columns
  end
  def description_width
      console_columns / 2 - OPERATOR_WIDTH - 4
  end

  def memory_operator do_write, name
    @memories[name] = @stack.last if do_write
    @stack.push @memories[name] if !do_write
  end
end
