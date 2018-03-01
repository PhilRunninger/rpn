require_relative 'number'
require_relative 'settings'
require 'json'
require 'launchy'
require 'io/console'

OPERATOR_WIDTH = 8
VALID_OPERATORS =   #{{{1
    [{'category' => 'Basic Arithmetic',
      'groups' => [{'function' => 'float_2_operator', 'operators' => {'+'    => 'Addition',
                                                                      '*'    => 'Multiplication',
                                                                      '-'    => 'Subtraction',
                                                                      '/'    => 'Division',
                                                                      'div'  => 'Integer division',
                                                                      '%'    => 'Modulus',
                                                                      '**'   => 'Exponentiation'}},
                   {'function' => 'float_1_operator', 'operators' => {'abs'  => 'Absolute value'}},
                   {'function' => 'custom_operator',  'operators' => {'chs'  => 'Negation'}}]},
     {'category' => 'Rounding',
      'groups' => [{'function' => 'float_1_operator', 'operators' => {'round'    => 'Round to nearest integer',
                                                                      'truncate' => 'Truncate to integer',
                                                                      'floor'    => 'Round down to nearest integer',
                                                                      'ceil'     => 'Round up to nearest integer'}}]},
     {'category' => 'Powers and Logarithms',
      'groups' => [{'function' => 'Math_1_operator',  'operators' => {'sqrt'  => 'Square Root',
                                                                      'exp'   => 'Raise e to the x power',
                                                                      'log'   => 'Natural Log of x',
                                                                      'log10' => 'Log (base 10) of x',
                                                                      'log2'  => 'Log (base 2) of x'}},
                   {'function' => 'custom_operator',  'operators' => {'\\'    => 'Reciprocal'}}]},
     {'category' => 'Trigonometric',
      'groups' => [{'function' => 'trig_operator',  'operators' => {'sin'   => 'Sine of x',
                                                                    'asin'  => 'Arcsine of x',
                                                                    'cos'   => 'Cosine of x',
                                                                    'acos'  => 'Arccosine of x',
                                                                    'tan'   => 'Tangent of x',
                                                                    'atan'  => 'Artangent of x'}}]},
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
     {'category' => 'Constants',
      'groups' => [{'function' => 'Math_constant',    'operators' => {'pi'  => '3.141592653....',
                                                                      'e'   => '2.718281828...',
                                                                      'phi' => '0.618033989...',
                                                                      'i' => 'sqrt(-1)'}}]},
     {'category' => 'Unit Conversion',
      'groups' => [{'function' => 'convert_unit', 'operators' => {'units' => 'Show list of available unit conversions.'}}],
      'suffix' => {'FROM>TO' => 'Convert x from \'FROM\' units to \'TO\' units, eg. mi>km'}},
     {'category' => 'Stack Manipulation',
      'groups' => [{'function' => 'custom_operator',  'operators' => {'copy' => 'Copy top value on stack',
                                                                      'del'  => 'Delete top value from stack',
                                                                      'cs'   => 'Clear the stack',
                                                                      'xy'   => 'Swap x and y'}}]},
     {'category' => 'Registers',
      'groups' => [{'function' => 'register_function', 'operators' => {'cr' => 'Clear all register values'}}],
      'suffix' => {'cr:foo' => 'Clear the register named \'foo\'',
                   '>foo'   => 'Copy x into the register named \'foo\'',
                   '>>foo'  => 'Copy the entire stack into the register named \'foo\'',
                   '<foo'   => 'Push register named \'foo\' onto the stack',
                   '<<foo'  => 'Replace stack with contents of register named \'foo\'',
                   '#HIDE#' => 'Register names consist of letters, numbers and underscores.'}},
     {'category' => 'Macros',
      'groups' => [{'function' => 'macro_function', 'operators' => {'lm' => 'List macro definitions',
                                                                    'cm' => 'Clear all macros'}}],
      'suffix' => {'bar(' => 'Start the definition of a macro named \'bar\'',
                   ')' => 'Finish the definition of the current macro',
                   'bar()' => 'Clear the macro named \'bar\''}},
     {'category' => 'Display Mode',
      'groups' => [{'function' => 'custom_operator', 'operators' => {'bin'  => 'Binary: 0b[01]+',
                                                                     'oct'  => 'Octal: 0[0-7]+',
                                                                     'hex'  => 'Hexadecimal: 0x[0-9a-f]+',
                                                                     'dec'  => 'Decimal (integer)',
                                                                     'norm' => 'Normal mode'}}],
      'suffix' => {'#HIDE1#' => 'BIN, OCT, HEX, and DEC modes display numbers as rounded integers. Use caution.',
                   '#HIDE2#' => 'Numbers of any base can be entered in any display mode.'}},
     {'category' => 'Angle Mode',
      'groups' => [{'function' => 'custom_operator', 'operators' => {'rad' => 'Switch to radians',
                                                                     'deg' => 'Switch to degrees'}}]},
     {'category' => 'Other',
      'groups' => [{'function' => 'custom_operator',  'operators' => {'ca' => 'Clear all',
                                                                      '<Enter>' => 'Exit calculator',
                                                                      'colors' => 'Change colors',
                                                                      '?'     => 'Display this list',
                                                                      '??'    => 'Google: RPN tutorials'}}]}
    ]

UNITS_CONVERSION = #{{{1
    [{'category'=>'length',
      'systems'=>[{'standard'=>'m',
                   'conversions'=>[{'unit'=>'nm',     'to_std'=>'1e9 /',                'from_std'=>'1e9 *'},
                                   {'unit'=>'micron', 'to_std'=>'1e6 /',                'from_std'=>'1e6 *'},
                                   {'unit'=>'mm',     'to_std'=>'1000 /',               'from_std'=>'1000 *'},
                                   {'unit'=>'cm',     'to_std'=>'100 /',                'from_std'=>'100 *'},
                                   {'unit'=>'m',      'to_std'=>'',                     'from_std'=>''},
                                   {'unit'=>'km',     'to_std'=>'1000 *',               'from_std'=>'1000 /'}]},
                  {'standard'=>'in',
                   'conversions'=>[{'unit'=>'in',     'to_std'=>'',                     'from_std'=>''},
                                   {'unit'=>'ft',     'to_std'=>'12 *',                 'from_std'=>'12 /'},
                                   {'unit'=>'yd',     'to_std'=>'3 * 12 *',             'from_std'=>'12 / 3 /'},
                                   {'unit'=>'mi',     'to_std'=>'5280 * 12 *',          'from_std'=>'12 / 5280 /'}]}],
      'translations'=>[{'from'=>'m',  'to'=>'in', 'translation'=>'100 * 2.54 /'},
                       {'from'=>'in', 'to'=>'m',  'translation'=>'2.54 * 100 /'}]},
     {'category'=>'time',
      'systems'=>[{'standard'=>'sec',
                   'conversions'=>[{'unit'=>'hr',  'to_std'=>'3600 *', 'from_std'=>'3600 /'},
                                   {'unit'=>'min', 'to_std'=>'60 *',   'from_std'=>'60 /'},
                                   {'unit'=>'sec', 'to_std'=>'',       'from_std'=>''}]}]},
     {'category'=>'speed',
      'systems'=>[{'standard'=>'m/sec',
                   'conversions'=>[{'unit'=>'mi/hr',  'to_std'=>'mi>m sec>hr', 'from_std'=>'m>mi hr>sec'},
                                   {'unit'=>'km/hr',  'to_std'=>'km>m sec>hr', 'from_std'=>'m>km hr>sec'},
                                   {'unit'=>'ft/sec', 'to_std'=>'ft>m',        'from_std'=>'m>ft'},
                                   {'unit'=>'m/sec',  'to_std'=>'',            'from_std'=>''},
                                   {'unit'=>'ft/hr',  'to_std'=>'ft>m sec>hr', 'from_std'=>'m>ft hr>sec'}]}]},
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
      'translations'=>[{'from'=>'kg', 'to'=>'lb', 'translation'=>'2.20462 *'},
                       {'from'=>'lb', 'to'=>'kg', 'translation'=>'2.20462 /'}]},
     {'category'=>'pressure',
      'systems'=>[{'standard'=>'pa',
                   'conversions'=>[{'unit'=>'psi',    'to_std'=>'6894.757280343134 *',       'from_std'=>'6894.757280343134 /'},
                                   {'unit'=>'kg/cm2', 'to_std'=>'0.000010197162129779282 /', 'from_std'=>'0.000010197162129779282 *'},
                                   {'unit'=>'kpa',    'to_std'=>'1000 *',                    'from_std'=>'1000 /'}]}]},
     {'category'=>'temperature',
      'systems'=>[{'standard'=>'C',
                   'conversions'=>[{'unit'=>'C', 'to_std'=>'',             'from_std'=>''},
                                   {'unit'=>'F', 'to_std'=>'32 - 5 * 9 /', 'from_std'=>'9 * 5 / 32 +'},
                                   {'unit'=>'K', 'to_std'=>'273.15 -',     'from_std'=>'273.15 +'}]}]},
     {'category'=>'angle',
      'systems'=>[{'standard'=>'rad',
                   'conversions'=>[{'unit'=>'rad', 'to_std'=>'',           'from_std'=>''},
                                   {'unit'=>'deg', 'to_std'=>'pi * 180 /', 'from_std'=>'180 * pi /'}]}]},
     {'category'=>'kitchen volumes',
      'systems'=>[{'standard'=>'tsp',
                   'conversions'=>[{'unit'=>'tsp',    'to_std'=>'',                     'from_std'=>''},
                                   {'unit'=>'tbsp',   'to_std'=>'3 *',                  'from_std'=>'3 /'},
                                   {'unit'=>'ounce',  'to_std'=>'3 2 * *',              'from_std'=>'3 2 * /'},
                                   {'unit'=>'cup',    'to_std'=>'16 * 3 *',             'from_std'=>'3 / 16 /'},
                                   {'unit'=>'pint',   'to_std'=>'2 16 3 * * *',         'from_std'=>'2 16 3 * * /'},
                                   {'unit'=>'quart',  'to_std'=>'2 2 16 3 * * * *',     'from_std'=>'2 2 16 3 * * * /'},
                                   {'unit'=>'gallon', 'to_std'=>'4 2 2 16 3 * * * * *', 'from_std'=>'4 2 2 16 3 * * * * /'}]}]}
    ]
BASES = {'bin'=>2, 'oct'=>8, 'dec'=>10, 'hex'=>16, 'norm'=>0}   # {{{1

class Processor   #{{{1
    attr_reader :stack, :registers, :macros, :recording, :settings
    attr_accessor :base, :angle

    def initialize settings_file   #{{{2
        @settings = Settings.new(settings_file)
        @stack = @settings.stack
        @registers = @settings.registers
        @macros = @settings.macros
        @recording = nil
        @base = @settings.base
        @angle = @settings.angle
    end

    def angle_mode   #{{{2
      return @angle
    end

    def radix   #{{{2
      return @base == 0 ? '' : BASES.key(@base).upcase
    end

    def format(value)   #{{{2
      return "[#{value.map{|v| v.format(@base)}.join(' ')}]" if value.kind_of?(Array)
      return value.format(@base)
    end

    def execute(text)   #{{{2
        saved_stack = @stack.dup
        begin
            text.split(' ').each do |value|
                number = parse_number(value)
                operator = parse_operator(value)
                register = parse_register(value)
                macro = parse_macro(value)
                unit_conversion = parse_unit_conversion(value)

                if macro
                    macro_function macro
                elsif !@recording.nil?
                    @macros[@recording] << value
                elsif number
                    @stack.push number
                elsif operator
                    send(operator['function'], value)
                elsif unit_conversion
                    convert_unit unit_conversion
                elsif register
                    register_function register
                else
                    raise NotImplementedError, "#{value} is not a valid number, operator, unit conversion, register, or macro."
                end
            end
        rescue Exception => exception
            @stack = saved_stack.dup
            raise exception
        end

        @stack.delete(nil)

        @settings.stack = @stack
        @settings.registers = @registers
        @settings.macros = @macros
        @settings.base = @base
        @settings.angle = @angle
        @settings.write

        @stack.last
    end

    def parse_number value   #{{{2
        begin
            return Number.new(value)
        rescue
            return nil
        end
    end

    def parse_operator value   #{{{2
        VALID_OPERATORS.map{|category| category['groups']}.flatten.find{|group| group['operators'].keys.include?(value)}
    end

    def parse_unit_conversion value   #{{{2
        value.match(/^([\w\/]+)>([\w\/]+)$/)
    end

    def parse_register value   #{{{2
        value.match(/^(cr:|>>?|<<?)(\w*[a-z]+\w*)$/)
    end

    def parse_macro value   #{{{2
      return nil if value.match(/^\w+$/) and @macros[value].nil?
      return value if value.match(/^\w+$/) and !@macros[value].nil?
      return value.match(/^((\w+)(\(?))?(\))?$/)
    end

    def float_1_operator operator   #{{{2
        x = @stack.pop
        @stack.push Number.new(x.value.send(operator.to_sym))
    end
    def float_2_operator operator   #{{{2
        x = @stack.pop
        y = @stack.pop
        @stack.push Number.new(y.value.send(operator.to_sym, x.value))
    end

    def int_1_operator operator   #{{{2
        x = @stack.pop.value.to_i
        @stack.push Number.new(x.send(operator.to_sym))
    end
    def int_2_operator operator   #{{{2
        x = @stack.pop.value.to_i
        y = @stack.pop.value.to_i
        @stack.push Number.new(y.send(operator.to_sym, x))
    end

    def Math_constant value   #{{{2
        case value
        when 'phi'
            @stack.push Number.new((Math.sqrt(5)+1)/2)
        when 'i'
            @stack.push Number.new(0+1i)
        else
            @stack.push Number.new(eval("Math::#{value.upcase}"))
        end
    end

    def Math_1_operator operator   #{{{2
        x = @stack.pop.value
        @stack.push Number.new(eval("Math.#{operator}(#{x})"))
    end

    def trig_operator operator   #{{{2
      case operator
      when 'sin', 'cos', 'tan'
        x = @stack.pop.value * (@angle == 'RAD' ? 1 : Math::PI / 180.0)
        @stack.push Number.new(eval("Math.#{operator}(#{x})"))
      when 'asin', 'acos', 'atan'
        x= @stack.pop.value
        @stack.push Number.new(eval("Math.#{operator}(#{x})") * (@angle == 'RAD' ? 1 : 180.0 / Math::PI))
      end
    end

    def statistics_operator operator   #{{{2
        case operator
        when '!'
            x = @stack.pop.value
            raise RangeError, "x must be non-negative." if x < 0
            @stack.push Number.new(factorial(x))
        when 'perm'
            x = @stack.pop.value
            y = @stack.pop.value
            @stack.push Number.new(factorial(y) / factorial(y - x))
        when 'comb'
            x = @stack.pop.value
            y = @stack.pop.value
            @stack.push Number.new(factorial(y) / (factorial(y - x) * factorial(x)))
        when 'sum'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(sum(@stack.map{|i| i.value}))]
        when 'product'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(product(@stack.map{|i| i.value}))]
        when 'count'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(@stack.size)]
        when 'mean'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(mean(@stack.map{|i| i.value}))]
        when 'median'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(median(@stack.map{|i| i.value}))]
        when 'std'
            @registers['sample'] = @stack.dup
            @stack = [Number.new(standard_deviation(@stack.map{|i| i.value}))]
        end
    end

    def factorial n
        n == 0 ? 1 : (1..n).reduce(:*)
    end
    def sum(a)
        a.inject(0){ |accum, i| accum + i}
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

    def custom_operator operator   #{{{2
        case operator
        when 'chs'
            @stack.push Number.new(-@stack.pop.value)
        when '\\'
            @stack.push Number.new(1/@stack.pop.value)
        when 'copy'
            @stack.push @stack.last
        when 'del'
            @stack.pop
        when 'cs'
            @stack = []
        when 'ca'
            @stack = []
            @macros = {}
            @registers = {}
            @base = 0
            @angle = 'DEG'
        when 'xy'
            x = @stack.pop
            y = @stack.pop
            @stack.push x
            @stack.push y
        when 'bin', 'oct', 'dec', 'hex', 'norm'
          @base = BASES[operator]
        when 'rad', 'deg'
          @angle = operator.upcase
        when 'colors'
          @settings.change_colors
        when '?'
            print "#{'─' * (console_columns - 1)}".colorize(@settings.color_help)
            VALID_OPERATORS.each{ |category|
                puts
                puts category['category'].colorize(@settings.color_help_heading)

                unless category['prefix'].nil?
                  category['prefix'].each{|part1, part2|
                      part1 = sprintf(" %#{OPERATOR_WIDTH}s  ", part1)
                      puts part1.colorize(@settings.color_help) +
                           part2.colorize(@settings.color_normal)
                  }
                end

                operators = category['groups'].inject({}) {|acc, op| acc.merge(op['operators'])}
                description_width = operators.values.inject(0) {|sum, text| [sum, text.length].max}

                total_width = 0
                operators.each{|op,description|
                    op = sprintf(" %#{OPERATOR_WIDTH}s  ", op)
                    description = sprintf("%-#{description_width}s", description)
                    if total_width + op.length + description.length + 3 < console_columns
                        print op.colorize(@settings.color_help) + description.colorize(@settings.color_normal)
                        total_width = total_width + op.length + description.length + 3
                    elsif total_width + op.length + description.rstrip.length + 3 < console_columns
                        puts op.colorize(@settings.color_help) + description.rstrip.colorize(@settings.color_normal)
                        total_width = 0
                    else
                        puts ''
                        print op.colorize(@settings.color_help) + description.colorize(@settings.color_normal)
                        total_width = op.length + description.length + 3
                    end
                }
                puts '' if total_width > 0

                unless category['suffix'].nil?
                  category['suffix'].each{|part1, part2|
                      part1 = sprintf(" %#{OPERATOR_WIDTH}s  ", part1 =~ /^\#HIDE.*\#$/ ? "" : part1)
                      puts part1.colorize(@settings.color_help) +
                           part2.colorize(@settings.color_normal)
                      }
                end
            }
            puts "#{'─' * (console_columns - 1)}".colorize(@settings.color_help)
        when '??'
            Launchy.open('https://www.google.com/webhp?ion=1&espv=2&es_th=1&ie=UTF-8#q=reverse%20polish%20notation%20tutorial&es_th=1')
        end
    end

    def register_function parts   #{{{2
        if parts.kind_of?(MatchData)
            name = parts.captures[1]
            raise ArgumentError, "The name #{name} is already used as an operator." if parse_operator(name)
            raise ArgumentError, "The name #{name} is already used to idenfity a macro." unless @macros[name].nil?
            case parts.captures[0]
            when '>'
                raise ArgumentError, "Nothing to save in register #{name}." if stack.size == 0
                @registers[name] = @stack.last
            when '>>'
                raise ArgumentError, "Nothing to save in register #{name}." if stack.size == 0
                @registers[name] = @stack.dup
            when '<'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                [@registers[name]].flatten.each{|value| @stack.push value}
            when '<<'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                @stack = [@registers[name]].flatten.dup
            when 'cr:'
                raise ArgumentError, "Register #{name} is not defined." if @registers[name].nil?
                @registers.delete(name)
            end
        else
            case parts
            when 'cr'
                @registers = {}
            end
        end
    end

    def convert_unit parts   #{{{2
      if parts.kind_of?(MatchData)
        from_units = parts.captures[0]
        to_units = parts.captures[1]
        all_units = UNITS_CONVERSION.map{ |x| x['systems'].map{ |y| y['conversions'].map{ |z| z['unit']} } }.flatten
        raise ArgumentError, "Invalid units: #{from_units}. Type 'units' to see valid units." unless all_units.include?(from_units)
        raise ArgumentError, "Invalid units: #{to_units}. Type 'units' to see valid units." unless all_units.include?(to_units)

        from_category = UNITS_CONVERSION.find{ |y| y['systems'].find{ |x| x['conversions'].find{ |z| z['unit']==from_units } } }
        to_category= UNITS_CONVERSION.find{ |y| y['systems'].find{ |x| x['conversions'].find{ |z| z['unit']==to_units } } }
        raise ArgumentError, "Incompatible units. Type 'units' to see valid units." unless from_category['category'] == to_category['category']

        f_standard = from_category['systems'].find{|x| x['conversions'].find{|y| y['unit']==from_units}}['standard']
        t_standard = to_category['systems'].find{|x| x['conversions'].find{|y| y['unit']==to_units}}['standard']

        from = from_category['systems'].find{|x| x['standard']==f_standard}['conversions'].find{|z| z['unit']==from_units}['to_std']
        translation = f_standard == t_standard ? '' : from_category['translations'].find{|x| x['from']==f_standard && x['to']==t_standard}['translation']
        to = from_category['systems'].find{|x| x['standard']==t_standard}['conversions'].find{|z| z['unit']==to_units}['from_std']
        execute "#{from} #{translation} #{to}"
      else
        case parts
        when 'units'
          puts "#{'─' * (console_columns - 1)}".colorize(@settings.color_help)
          UNITS_CONVERSION.each{ |category|
            puts "#{category['category']}: ".colorize(@settings.color_help_heading) +
                 "#{category['systems'].map{|s| s['conversions'].map{|u| u['unit']}.join(', ')}.join(', ')}".colorize(@settings.color_normal)
          }
          puts "#{'─' * (console_columns - 1)}".colorize(@settings.color_help)
        end
      end
    end

    def macro_function parts  #{{{2
      if parts.kind_of?(MatchData)
        name = parts.captures[1]
        start_of_macro = !parts.captures[2].nil?
        end_of_macro = !parts.captures[3].nil?

        macro_function name if !name.nil? and !start_of_macro and !end_of_macro

        if !name.nil?
          raise ArgumentError, "The name #{name} is already used as an operator." if parse_operator(name)
          raise ArgumentError, "The name #{name} is already used to idenfity a register." unless @registers[name].nil?
          @recording = name
          @macros[@recording] = []
        end

        @macros.delete(@recording) if end_of_macro and @macros[@recording] == []
        @recording = nil if end_of_macro
      else
        case parts
        when 'lm'
          width = @macros.map{|macro_name,definition| macro_name.length}.max
          @macros.map{|macro_name,definition|
            macro_name = sprintf("  %#{width}s ", macro_name)
            print "#{macro_name} ".colorize(settings.color_help)
            puts "#{definition.join(' ')}".colorize(settings.color_normal)
          }
        when 'cm'
          @macros = {}
        else
            if @recording
                raise ArgumentError, "A macro cannot call itself. It will never finish." if parts == @recording
                @macros[@recording] << parts
            else
                execute @macros[parts].join(' ') unless @macros[parts].nil?
            end
        end
      end
    end

    def console_columns   #{{{2
      _, columns = IO.console.winsize
      [columns, 60].max
    end

    #}}}
end

#}}}
# vim:ft=ruby foldmethod=marker sw=4
