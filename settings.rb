require 'json'

class Settings
  attr_reader :hash

  def initialize(settings_file)
    @settings_file = settings_file
    @hash = {}
    @hash = JSON.parse(File.read(settings_file)) if File.exist?(settings_file)
  end

  def write
    File.open(@settings_file,'w') {|f| f.write(JSON.pretty_generate(@hash))}
  end

  def stack= stack
    @hash['stack'] = stack.map{|x| x.to_h}
  end
  def stack
    return [] if @hash['stack'].nil?
    return @hash['stack'].map{|x| Number.from_h(x)}
  end

  def registers= registers
    @hash['registers'] = Hash[registers.map{|k,v| [k,v.kind_of?(Array) ? v.map{|n| n.to_h} : v.to_h]}]
  end
  def registers
    return {} if @hash['registers'].nil?
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

  def color_normal= color_normal
    @hash['color_normal'] = color_normal
  end
  def color_normal
    (@hash['color_normal'] || 'default').to_sym
  end

  def color_error= color_error
    @hash['color_error'] = color_error
  end
  def color_error
    (@hash['color_error'] || 'default').to_sym
  end

  def color_title= color_title
    @hash['color_title'] = color_title
  end
  def color_title
    (@hash['color_title'] || 'default').to_sym
  end

  def color_register= color_register
    @hash['color_register'] = color_register
  end
  def color_register
    (@hash['color_register'] || 'default').to_sym
  end

  def color_help_heading= color_help_heading
    @hash['color_help_heading'] = color_help_heading
  end
  def color_help_heading
    (@hash['color_help_heading'] || 'default').to_sym
  end

  def color_help= color_help
    @hash['color_help'] = color_help
  end
  def color_help
    (@hash['color_help'] || 'default').to_sym
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
      end while (item =~ /[0-5\r]/).nil?
      print item

      if item != "\r"
        print '  '
        colors.each_index{|i| print "#{i.to_s(16).upcase}•".colorize(colors[i])}
        print '▷ '
        begin
          color = STDIN.getch.upcase
        end while (color =~ /[0-9A-F\r]/).nil?
        puts color

        send(items.values[item.to_i]+'=', colors[color.to_i(16)].to_sym) unless color == "\r"
      else
        puts ''
      end
    end while item != "\r" and color != "\r"
    puts ''
  end
end
