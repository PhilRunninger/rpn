$LOAD_PATH.unshift File.dirname($0)

require_relative 'processor'
require_relative 'ansi_colors'

processor = Processor.new

puts "#{GREEN_TEXT}RPN Calculator, ©2014, Phil Runninger #{CYAN_TEXT}._.-._.-._.-._.-._.-._. Enter ? for help."
print "#{GREEN_TEXT} > #{RESET_COLORS}"
input = gets.chomp
while input > ''

  begin
    processor.execute input
  rescue Exception => msg
    puts "#{RED_TEXT}#{msg}"
  end
  print "#{GRAY_TEXT} "
  processor.stack.each{|val|
    print "#{val.round} " if val.integer?
    print "#{val} " unless val.integer?
  }

  print "#{GREEN_TEXT}> #{RESET_COLORS}"
  input = gets.chomp
end
