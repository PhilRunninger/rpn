$LOAD_PATH.unshift File.dirname($0)

require 'clipboard'
require_relative 'processor'
require_relative 'ansi_colors'

processor = Processor.new

puts "#{GREEN_TEXT}RPN Calculator, ©2014, Phil Runninger #{CYAN_TEXT}======================= Enter ? for help."
print "#{GREEN_TEXT} » #{RESET_COLORS}"
input = gets.chomp
while input > ''

  begin
    processor.execute input
  rescue Exception => msg
    puts "#{RED_TEXT}#{msg}"
  end
  processor.stack.each{|val|
    print "#{GRAY_TEXT} #{val % 1 == 0 ? val.round : val} "
  }

  print "#{GREEN_TEXT}» #{RESET_COLORS}"
  input = gets.chomp
end

puts "#{CYAN_TEXT}========================================================= Thanks for using rpn.#{RESET_COLORS}"
answer = processor.stack.last
if !answer.nil?
  answer = (answer % 1 == 0 ? answer.round : answer)
  Clipboard.copy answer.to_s
  puts " #{GRAY_TEXT}#{answer} #{RESET_COLORS}was copied to the clipboard."
end
puts " "
