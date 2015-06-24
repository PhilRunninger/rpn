require_relative 'common'

OPERATOR_WIDTH = 8
VALID_OPERATORS = [{'category' => 'Basic Arithmetic',
                    'groups' => [{'function' => 'float_2_operator', 'operators' => {'+'    => 'Addition',
                                                                                    '*'    => 'Multiplication',
                                                                                    '-'    => 'Subtraction',
                                                                                    '/'    => 'Division',
                                                                                    'div'  => 'Integer portion of division',
                                                                                    '%'    => 'Modulus',
                                                                                    '**'   => 'Exponentiation'}},
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
                    'groups' => [{'function' => 'Math_1_operator',  'operators' => {'sin'   => 'Sine of x in radians',
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
                    'groups' => [{'function' => 'int_2_operator',   'operators' => {'&'  => 'AND',
                                                                                    '|'  => 'OR',
                                                                                    '^'  => 'XOR',
                                                                                    '<<' => 'Left Shift',
                                                                                    '>>' => 'Right Shift'}},
                                 {'function' => 'int_1_operator',   'operators' => {'~'  => "1's complement"}}]},
                   {'category' => 'Unit Conversion',
                    'groups' => [{'function' => 'convert_unit', 'operators' => {'units' => 'Show list of available unit conversions.'}}],
                    'suffix' => {'FROM>TO' => 'Convert x from \'FROM\' units to \'TO\' units, eg. mi>km'}},
                   {'category' => 'Stack Manipulation',
                    'groups' => [{'function' => 'custom_operator',  'operators' => {'copy' => 'Copy top value on stack',
                                                                                    'del'  => 'Delete top value from stack',
                                                                                    'cs'   => 'Clear the stack',
                                                                                    'xy'   => 'Swap x and y'}}]},
                   {'category' => 'Registers',
                    'groups' => [{'function' => 'register_function', 'operators' => {'cr' => 'Clear register values'}}],
                    'suffix' => {'foo='  => 'Copy x into the register named \'foo\'',
                                 'foo==' => 'Copy the entire stack into the register named \'foo\'',
                                 'foo'   => 'Push register named \'foo\' onto the stack',
                                 '=foo'  => 'Push register named \'foo\' onto the stack',
                                 '==foo' => 'Replace stack with contents of register named \'foo\'',
                                 ''      => 'Register names consist of letters, numbers and underscores.'}},
                   {'category' => 'Help',
                    'groups' => [{'function' => 'custom_operator',  'operators' => {'?' => 'Display this list'}}]}
            ]

# The conversion expressions are as follows for each category:
#   'to_std' converts 'unit' to 'standard'
#   'from_std' converts 'standard' to 'unit'
UNITS_CONVERSION = [{'category'=>'length',
                     'systems'=>[{'standard'=>'m',
                                  'conversions'=>[{'unit'=>'nm',     'to_std'=>'1e9 /',                'from_std'=>'1e9 *'},
                                                  {'unit'=>'micron', 'to_std'=>'1e6 /',                'from_std'=>'1e6 *'},
                                                  {'unit'=>'mm',     'to_std'=>'1000 /',               'from_std'=>'1000 *'},
                                                  {'unit'=>'cm',     'to_std'=>'100 /',                'from_std'=>'100 *'},
                                                  {'unit'=>'m',      'to_std'=>'',                     'from_std'=>''},
                                                  {'unit'=>'km',     'to_std'=>'1000 *',               'from_std'=>'1000 /'},
                                                  {'unit'=>'ly',     'to_std'=>'9.4607304725808e15 *', 'from_std'=>'9.4607304725808e15 /'}]},
                                 {'standard'=>'in',
                                  'conversions'=>[{'unit'=>'in',     'to_std'=>'',                     'from_std'=>''},
                                                  {'unit'=>'ft',     'to_std'=>'12 *',                 'from_std'=>'12 /'},
                                                  {'unit'=>'yd',     'to_std'=>'3 * 12 *',             'from_std'=>'12 / 3 /'},
                                                  {'unit'=>'mi',     'to_std'=>'5280 * 12 *',          'from_std'=>'12 / 5280 /'}]}],
                     'translations'=>[{'to_std'=>'m',  'from_std'=>'in', 'translation'=>'100 * 2.54 /'},
                                      {'to_std'=>'in', 'from_std'=>'m',  'translation'=>'2.54 * 100 /'}]},
                    {'category'=>'time',
                     'systems'=>[{'standard'=>'sec',
                                  'conversions'=>[{'unit'=>'hr', 'to_std'=>'3600 *', 'from_std'=>'3600 /'},
                                                  {'unit'=>'min', 'to_std'=>'60 *', 'from_std'=>'60 /'},
                                                  {'unit'=>'sec', 'to_std'=>'', 'from_std'=>''}]}]},
                    {'category'=>'speed',
                     'systems'=>[{'standard'=>'m/sec',
                                  'conversions'=>[{'unit'=>'mi/hr', 'to_std'=>'mi>m sec>hr', 'from_std'=>'m>mi hr>sec'},
                                                  {'unit'=>'km/hr', 'to_std'=>'km>m sec>hr', 'from_std'=>'m>km hr>sec'},
                                                  {'unit'=>'ft/sec', 'to_std'=>'ft>m', 'from_std'=>'m>ft'},
                                                  {'unit'=>'m/sec', 'to_std'=>'', 'from_std'=>''},
                                                  {'unit'=>'ft/hr', 'to_std'=>'ft>m sec>hr', 'from_std'=>'m>ft hr>sec'}]}]},
                    {'category'=>'weight (force)',
                     'systems'=>[{'standard'=>'kg',
                                  'conversions'=>[{'unit'=>'mg',  'to_std'=>'1000000 /',       'from_std'=>'1000000 *'},
                                                  {'unit'=>'g',   'to_std'=>'1000 /',          'from_std'=>'1000 *'},
                                                  {'unit'=>'kg',  'to_std'=>'',                'from_std'=>''},
                                                  {'unit'=>'N',   'to_std'=>'9.80665002864 /', 'from_std'=>'9.80665002864 *'}]},
                                 {'standard'=>'lb',
                                  'conversions'=>[{'unit'=>'oz',  'to_std'=>'16 /',            'from_std'=>'16 *'},
                                                  {'unit'=>'lb',  'to_std'=>'',                'from_std'=>''},
                                                  {'unit'=>'ton', 'to_std'=>'2000 *',          'from_std'=>'2000 /'}]}],
                     'translations'=>[{'to_std'=>'kg', 'from_std'=>'lb', 'translation'=>'2.20462 *'},
                                      {'to_std'=>'lb', 'from_std'=>'kg', 'translation'=>'2.20462 /'}]},
                    {'category'=>'pressure',
                     'systems'=>[{'standard'=>'pa',
                                  'conversions'=>[{'unit'=>'psi',    'to_std'=>'6894.757280343134 *',       'from_std'=>'6894.757280343134 /'},
                                                  {'unit'=>'kg/cm2', 'to_std'=>'0.000010197162129779282 /', 'from_std'=>'0.000010197162129779282 *'},
                                                  {'unit'=>'kpa',    'to_std'=>'1000 *',                    'from_std'=>'1000 /'}]}]},
                    {'category'=>'kitchen volumes',
                     'systems'=>[{'standard'=>'tsp',
                                  'conversions'=>[{'unit'=>'tsp',    'to_std'=>'',                     'from_std'=>''},
                                                  {'unit'=>'tbsp',   'to_std'=>'3 *',                  'from_std'=>'3 /'},
                                                  {'unit'=>'ounce',  'to_std'=>'3 2 * *',              'from_std'=>'3 2 * /'},
                                                  {'unit'=>'cup',    'to_std'=>'16 * 3 *',             'from_std'=>'3 / 16 /'},
                                                  {'unit'=>'pint',   'to_std'=>'2 16 3 * * *',         'from_std'=>'2 16 3 * * /'},
                                                  {'unit'=>'quart',  'to_std'=>'2 2 16 3 * * * *',     'from_std'=>'2 2 16 3 * * * /'},
                                                  {'unit'=>'gallon', 'to_std'=>'4 2 2 16 3 * * * * *', 'from_std'=>'4 2 2 16 3 * * * * /'}]}]},
                    {'category'=>'temperature',
                     'systems'=>[{'standard'=>'C',
                                  'conversions'=>[{'unit'=>'C', 'to_std'=>'',             'from_std'=>''},
                                                  {'unit'=>'F', 'to_std'=>'32 - 5 * 9 /', 'from_std'=>'9 * 5 / 32 +'},
                                                  {'unit'=>'K', 'to_std'=>'273.15 -',     'from_std'=>'273.15 +'}]}]},
                    {'category'=>'angle',
                     'systems'=>[{'standard'=>'rad',
                                  'conversions'=>[{'unit'=>'rad', 'to_std'=>'',           'from_std'=>''},
                                                  {'unit'=>'deg', 'to_std'=>'pi * 180 /', 'from_std'=>'180 * pi /'}]}]}
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
                unit_conversion = parse_unit_conversion(value)

                if number
                    @stack.push number
                elsif operator
                    send(operator['function'], value)
                    elsif unit_conversion
                      convert_unit unit_conversion
                elsif register
                    register_function register
                else
                    raise NotImplementedError, "#{value} is not a valid number, operator, or register."
                end
            end
        rescue Exception => exception
            @stack = save_stack.dup
            raise exception
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

    def parse_unit_conversion value
        value.match(/^([\w\/]+)>([\w\/]+)$/)
    end

    def parse_register value
        value.match(/^((=?=?)(\w*[a-z]+\w*)|(\w*[a-z]+\w*)(==?))$/)
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
        when 'chs'
            @stack.push (-@stack.pop)
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
            puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}"
            VALID_OPERATORS.each{ |category|
                puts "#{BLUE_TEXT}#{category['category']}"

                category['prefix'].each{|part1, part2| printf " #{CYAN_TEXT}%#{OPERATOR_WIDTH}s  #{GRAY_TEXT}%-#{description_width}s\n", part1, part2 } if !category['prefix'].nil?

                operators = category['groups'].inject({}) {|acc, op| acc.merge(op['operators'])}
                description_width = operators.values.inject(0) {|sum, text| [sum, text.length].max}

                total_width = 0
                operators.each{|op,description|
                    text = sprintf " #{CYAN_TEXT}%#{OPERATOR_WIDTH}s  #{GRAY_TEXT}%-#{description_width}s", op, description
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
            puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}"
        end
    end

    def register_function parts
        if parts.kind_of?(MatchData)
            operator = "#{parts.captures[1]}[name]#{parts.captures[4]}"
            name =  parts.captures[2] || parts.captures[3]
            raise ArgumentError, "The operator #{name} cannot be used as a register name." if parse_operator(name)
            case operator
            when '[name]='
                raise ArgumentError, "Nothing to save in register #{name}." if stack.size == 0
                @registers[name] = @stack.last
            when '[name]=='
                raise ArgumentError, "Nothing to save in register #{name}." if stack.size == 0
                @registers[name] = @stack.dup
            when '[name]'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                [@registers[name]].flatten.each{|value| @stack.push value}
            when '=[name]'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                [@registers[name]].flatten.each{|value| @stack.push value}
            when '==[name]'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                @stack = [@registers[name]].flatten.dup
            end
        else
            case parts
            when 'cr'
                @registers = {}
            end
        end
    end

    def convert_unit parts
      if parts.kind_of?(MatchData)
        from_units = parts.captures[0]
        to_units = parts.captures[1]
        all_units = UNITS_CONVERSION.map{|x| x['systems'].map{|y| y['conversions'].map{|z| z['unit']}}}.flatten
        raise ArgumentError, "Invalid units: #{from_units}. Type 'units' to see valid units." unless all_units.include?(from_units)
        raise ArgumentError, "Invalid units: #{to_units}. Type 'units' to see valid units." unless all_units.include?(to_units)

        f = UNITS_CONVERSION.find{|y| y['systems'].find{|x| x['conversions'].find{|z| z['unit']==from_units}}}
        t = UNITS_CONVERSION.find{|y| y['systems'].find{|x| x['conversions'].find{|z| z['unit']==to_units}}}
        raise ArgumentError, "Incompatible units. Type 'units' to see valid units." unless f['category'] == t['category']

        f_standard = f['systems'].find{|x| x['conversions'].find{|y| y['unit']==from_units}}['standard']
        t_standard = t['systems'].find{|x| x['conversions'].find{|y| y['unit']==to_units}}['standard']

        from = f['systems'].find{|x| x['standard']==f_standard}['conversions'].find{|z| z['unit']==from_units}['to_std']
        translation = f_standard == t_standard ? '' : f['translations'].find{|x| x['to_std']==f_standard && x['from_std']==t_standard}['translation']
        to = f['systems'].find{|x| x['standard']==t_standard}['conversions'].find{|z| z['unit']==to_units}['from_std']
        execute "#{from} #{translation} #{to}"
      else
        case parts
        when 'units'
          puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}"
          UNITS_CONVERSION.each{ |category|
            puts "#{BLUE_TEXT}#{category['category']}:  #{GRAY_TEXT}#{category['systems'].map{|s| s['conversions'].map{|u| u['unit']}.join(', ')}.join(', ')}"
          }
          puts "#{CYAN_TEXT}#{'─' * (console_columns - 1)}"
        end
      end
    end

end
