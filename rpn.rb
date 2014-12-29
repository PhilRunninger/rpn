$LOAD_PATH.unshift File.dirname($0)

require 'clipboard'
require_relative 'processor'
require_relative 'ansi_colors'

processor = Processor.new

puts "#{TITLE_COLOR}RPN Calculator, ©2014, Phil Runninger #{HIGHLIGHT_COLOR}======================= Enter ? for help."
print "#{TITLE_COLOR} » #{RESET_COLORS}"
input = gets.chomp
while input > ''
  begin
    answer = processor.execute(input)
  rescue Exception => msg
    answer = nil
    puts "#{ERROR_COLOR}#{msg}"
  end
  processor.registers.each{|name, value| print "#{RESET_COLORS} #{name}=#{value % 1 == 0 ? value.round : value} " }
  print "#{TITLE_COLOR}|" if processor.registers != {}
  processor.stack.each{|value| print "#{HELP_TEXT} #{value % 1 == 0 ? value.round : value} " }
  print "#{TITLE_COLOR}» #{RESET_COLORS}"
  input = gets.chomp
end

puts "#{HIGHLIGHT_COLOR}========================================================= Thanks for using rpn.#{RESET_COLORS}"
if !answer.nil?
  answer = (answer % 1 == 0 ? answer.round : answer)
  Clipboard.copy answer.to_s
  puts " #{HELP_TEXT}#{answer} #{RESET_COLORS}was copied to the clipboard."
end
puts " "
sleep 1
