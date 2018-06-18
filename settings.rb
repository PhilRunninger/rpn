require 'json'

class Settings
  attr_reader :hash

  def initialize(settings_file)
    @settings_file = settings_file
    @hash = {}
    begin
      @hash = JSON.parse(File.read(settings_file)) if File.exist?(settings_file)
    rescue Exception
      @hash = {}
    end
  end

  def write
    File.open(@settings_file,'w') {|f| f.write(JSON.pretty_generate(@hash))} if @settings_file
  end

  def stack= stack
    @hash['stack'] = stack.map{|x| x.to_h}
  end
  def stack
    return [] unless @hash['stack']
    return @hash['stack'].map{|x| Number.from_h(x)}
  end

  def registers= registers
    @hash['registers'] = Hash[registers.map{|k,v| [k,v.kind_of?(Array) ? v.map{|n| n.to_h} : v.to_h]}]
  end
  def registers
    return {} unless @hash['registers']
    return Hash[@hash['registers'].map{|k,v| [k,v.kind_of?(Array) ? v.map{|n| Number.from_h(n)} : Number.from_h(v)]}]
  end

  def macros= macros
    @hash['macros'] = macros
  end
  def macros
    @hash['macros'] || {}
  end

  def base= base
    @hash['base'] = base
  end
  def base
    @hash['base'] || 0
  end

  def angle= angle
    @hash['angle'] = angle
  end
  def angle
    @hash['angle'] || 'DEG'
  end

  def get_color(type)
    return :default unless @hash['colors']
    (@hash['colors'][type] || 'default').to_sym
  end

  def set_color(type, value)
    @hash['colors'] = {} unless @hash['colors']
    @hash['colors'][type] = value
  end

  def color_normal= value
    set_color 'normal', value
  end
  def color_normal
    get_color 'normal'
  end

  def color_error= value
    set_color 'error', value
  end
  def color_error
    get_color 'error'
  end

  def color_title= value
    set_color 'title', value
  end
  def color_title
    get_color 'title'
  end

  def color_register= value
    set_color 'register', value
  end
  def color_register
    get_color 'register'
  end

  def color_help_heading= value
    set_color 'help_heading', value
  end
  def color_help_heading
    get_color 'help_heading'
  end

  def color_help= value
    set_color 'help', value
  end
  def color_help
    get_color 'help'
  end

  def change_colors
    items = {'Title'=>'color_title',
             'Normal'=>'color_normal',
             'Error'=>'color_error',
             'Register'=>'color_register',
             'Help Heading'=>'color_help_heading',
             'Help'=>'color_help'}
    colors = [:black, :light_black, :red, :light_red, :green, :light_green, :yellow, :light_yellow, :blue, :light_blue, :magenta, :light_magenta, :cyan, :light_cyan, :white, :light_white]
    puts ''
    puts 'Choose an item then a color. Press Enter to go back to the calculator.'
    begin
      print ' '
      items.each_with_index{|(item,color_function),index| print " #{index}=#{item}".colorize(send(color_function))}
      print '▷ '
      begin
        item = STDIN.getch.upcase
      end until item =~ /[0-5\r]/
      print item

      if item == "\r"
        puts ''
      else
        print '  '
        colors.each_index{|i| print "#{i.to_s(16).upcase}•".colorize(colors[i])}
        print '▷ '
        begin
          color = STDIN.getch.upcase
        end until color =~ /[0-9A-F\r]/
        puts color

        send(items.values[item.to_i]+'=', colors[color.to_i(16)].to_sym) unless color == "\r"
      end
    end while item != "\r" and color != "\r"
    puts ''
  end
end
