$LOAD_PATH.unshift File.dirname($0)

require 'clipboard'
require_relative 'processor'
require_relative 'ansi_colors'

processor = Processor.new

puts "#{TITLE_COLOR}RPN Calculator, ©2014, Phil Runninger #{HIGHLIGHT_COLOR}#{'═' * (console_columns - 57)} Enter ? for help."
print "#{TITLE_COLOR}► #{NORMAL_COLOR}"
input = gets.chomp
while input > ''
    begin
        answer = processor.execute(input)
    rescue Exception => msg
        answer = nil
        puts "#{ERROR_COLOR}#{msg}"
    end
    processor.registers.each{|name, value| print "#{NORMAL_COLOR}#{name}#{HELP_TEXT}=#{NORMAL_COLOR}#{value % 1 == 0 ? value.round : value} " }
    print "#{TITLE_COLOR}∫ " if processor.registers != {}
    processor.stack.each{|value| print "#{HELP_TEXT}#{value % 1 == 0 ? value.round : value} " }
    print "#{TITLE_COLOR}► #{NORMAL_COLOR}"
    input = gets.chomp
end

puts "#{HIGHLIGHT_COLOR}#{'═' * (console_columns - 23)} Thanks for using rpn."
if !answer.nil?
    answer = (answer % 1 == 0 ? answer.round : answer)
    Clipboard.copy answer.to_s
    puts "#{HELP_TEXT}#{answer} #{NORMAL_COLOR}was copied to the clipboard."
end
puts "#{RESET_COLORS} "
sleep 1
