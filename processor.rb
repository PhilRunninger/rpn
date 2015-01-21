require_relative 'common'

OPERATOR_WIDTH = 8
VALID_OPERATORS = [{'category' => 'Basic Arithmetic',
                    'groups' => [{'function' => 'float_2_operator', 'operators' => {'+'   => 'Addition',
                                                                                    '*'   => 'Multiplication',
                                                                                    '-'   => 'Subtraction',
                                                                                    '/'   => 'Division',
                                                                                    'div' => 'Integer portion of division',
                                                                                    '%'   => 'Modulus',
                                                                                    '**'  => 'Exponentiation'}},
                                 {'function' => 'float_1_operator', 'operators' => {'abs'  => 'Absolute value of x'}},
                                 {'function' => 'custom_operator',  'operators' => {'chs'  => 'Change the sign of x'}}]},
                   {'category' => 'Rounding',
                    'groups' => [{'function' => 'float_1_operator', 'operators' => {'round'    => 'Round to nearest integer',
                                                                                    'truncate' => 'Truncate to integer',
                                                                                    'floor'    => 'Round down to nearest integer',
                                                                                    'ceil'     => 'Round up to nearest integer'}}]},
                   {'category' => 'Constants',
                    'groups' => [{'function' => 'Math_constant',    'operators' => {'pi' => '3.141592653....',
                                                                                    'e'  => '2.718281828...'}}]},
                   {'category' => 'Powers and Logarithms',
                    'groups' => [{'function' => 'Math_1_operator',  'operators' => {'sqrt'  => 'Square Root',
                                                                                    'exp'   => 'Raise e to the x power',
                                                                                    'log'   => 'Natural Log of x',
                                                                                    'log10' => 'Log (base 10) of x',
                                                                                    'log2'  => 'Log (base 2) of x'}},
                                 {'function' => 'custom_operator',  'operators' => {'\\'    => 'Reciprocal'}}]},
                   {'category' => 'Trigonometric',
                    'groups' => [{'function' => 'custom_operator',  'operators' => {'rad'   => 'Convert degrees to radians',
                                                                                    'deg'   => 'Convert radians to degrees'}},
                                 {'function' => 'Math_1_operator',  'operators' => {'sin'   => 'Sine of x in radians',
                                                                                    'asin'  => 'Arcsine in radians of x',
                                                                                    'cos'   => 'Cosine of x in radians',
                                                                                    'acos'  => 'Arccosine in radians of x',
                                                                                    'tan'   => 'Tangent of x in radians',
                                                                                    'atan'  => 'Artangent in radians of x'}}]},

                   {'category' => 'Statistics',
                    'groups' => [{'function' => 'statistics_operator', 'operators' => {'!'       => 'Factorial',
                                                                                       'perm'    => 'Permutation(Y, X)',
                                                                                       'comb'    => 'Combination(Y, X)',
                                                                                       'sum'     => 'Sum of the stack',
                                                                                       'product' => 'Product of the stack',
                                                                                       'mean'    => 'Mean average',
                                                                                       'median'  => 'Median average',
                                                                                       'std'     => 'Standard Deviation',
                                                                                       'count'   => 'Size of the stack'}}]},
                   {'category' => 'Bitwise',
                    'groups' => [{'function' => 'int_2_operator',   'operators' => {'&'     => 'AND',
                                                                                    '|'        => 'OR',
                                                                                    '^'     => 'XOR',
                                                                                    '<<'       => 'Left Shift',
                                                                                    '>>'    => 'Right Shift'}},
                                 {'function' => 'int_1_operator',   'operators' => {'~'     => "1's complement"}}]},
                   {'category' => 'Stack Manipulation',
                    'groups' => [{'function' => 'custom_operator',  'operators' => {'copy'  => 'Copy top value on stack',
                                                                                    'del'      => 'Delete top value from stack',
                                                                                    'cs'    => 'Clear the stack',
                                                                                    'xy'       => 'Swap x and y'}}]},
                   {'category' => 'Registers',
                    'groups' => [{'function' => 'register_function', 'operators' => {'cr'    => 'Clear register values'}}],
                    'suffix' => {'@foo' => 'Copy x into the register named \'foo\'', 
                                 '@@foo' => 'Copy the entire stack into the register named \'foo\'', 
                                 '<foo' => 'Push register named \'foo\' onto the stack',
                                 '<<foo' => 'Replace stack with contents of register named \'foo\'',
                                 '' => 'Register names consist of letters, numbers and underscores.'}},

                   {'category' => 'Help',
                    'groups' => [{'function' => 'custom_operator',  'operators' => {'?'     => 'Display this list'}}]}
            ]

class Processor
    attr_reader :stack, :registers

    def initialize
        @stack = []
        @registers = {}
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
                    register_function register
                else
                    raise NotImplementedError, "#{value} is not a valid number, operator, or register."
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
        VALID_OPERATORS.map{|category| category['groups']}.flatten.find{|group| group['operators'].keys.include?(value)}
    end

    def parse_register value
        name = value.match(/^(<<?|@@?)(\w+)$/i)
        return nil if name.nil? or !parse_operator(name.captures[1]).nil?
        return name unless name.captures[0].start_with?('<') and @registers[name.captures[1]].nil?
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

    def statistics_operator operator
        case operator
        when '!'
            x = @stack.pop
            raise RangeError, "x must be non-negative." if x < 0
            @stack.push factorial(x)
        when 'perm'
            x = @stack.pop
            y = @stack.pop
            @stack.push factorial(y) / factorial(y - x)
        when 'comb'
            x = @stack.pop
            y = @stack.pop
            @stack.push factorial(y) / (factorial(y - x) * factorial(x))
        when 'sum'
            @registers['sample'] = @stack.dup
            @stack = [sum(@stack)]
        when 'product'
            @registers['sample'] = @stack.dup
            @stack = [product(@stack)]
        when 'count'
            @registers['sample'] = @stack.dup
            @stack = [@stack.size]
        when 'mean'
            @registers['sample'] = @stack.dup
            @stack = [mean(@stack)]
        when 'median'
            @registers['sample'] = @stack.dup
            @stack = [median(@stack)]
        when 'std'
            @registers['sample'] = @stack.dup
            @stack = [standard_deviation(@stack)]
        end
    end

    def factorial n
        n == 0 ? 1 : (1..n).reduce(:*)
    end
    def sum(a)
        a.inject(0){ |accum, i| accum + i }
    end
    def product(a)
        a.inject(1){ |accum, i| accum * i }
    end
    def mean(a)
        sum(a) / a.length.to_f
    end
    def median(a)
        sorted = a.sort
        len = sorted.length
        (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
    end
    def sample_variance(a)
        m = mean(a)
        sum = a.inject(0){ |accum, i| accum + (i - m) ** 2 }
        sum / (a.length - 1).to_f
    end
    def standard_deviation(a)
        Math.sqrt(sample_variance(a))
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
        when 'xy'
            x = @stack.pop
            y = @stack.pop
            @stack.push x
            @stack.push y
        when '?'
            puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}#{BROWN_TEXT}"
            VALID_OPERATORS.each{ |category|
                puts "#{BLUE_TEXT}#{category['category']}"

                category['prefix'].each{|part1, part2| printf " #{CYAN_TEXT}%#{OPERATOR_WIDTH}s  #{GRAY_TEXT}%-#{description_width}s\n", part1, part2 } if !category['prefix'].nil?

                operators = category['groups'].inject({}) {|acc, x| acc.merge(x['operators'])}
                description_width = operators.values.inject(0) {|sum, text| [sum, text.length].max}

                total_width = 0
                operators.each{|operator,description|
                    text = sprintf " #{CYAN_TEXT}%#{OPERATOR_WIDTH}s  #{GRAY_TEXT}%-#{description_width}s", operator, description
                    if total_width + text.length - 10 < console_columns
                        print "#{text}"
                        total_width = total_width + text.length - 10
                    elsif total_width + text.rstrip.length - 10 < console_columns
                        puts "#{text.rstrip}"
                        total_width = 0
                    else
                        puts ""
                        print "#{text}"
                        total_width = text.length - 10
                    end
                }
                puts "" if total_width > 0

                category['suffix'].each{|part1, part2| printf " #{CYAN_TEXT}%#{OPERATOR_WIDTH}s  #{GRAY_TEXT}%-#{description_width}s\n", part1, part2 } if !category['suffix'].nil?
            }
            puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}#{BROWN_TEXT}"
        end
    end

    def register_function operator
        if operator.kind_of?(MatchData)
            prefix = operator.captures[0]
            name = operator.captures[1]
            @registers[name] = @stack.last if prefix == '@'
            @registers[name] = @stack.dup if prefix == '@@'
            [@registers[name]].flatten.each{|value| @stack.push value} if prefix == '<'
            @stack = [@registers[name]].flatten.dup if prefix == '<<'
        else
            case operator
            when 'cr'
                @registers = {}
            end
        end
    end
end
