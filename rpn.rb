$LOAD_PATH.unshift File.dirname($0)

require "readline"
require 'clipboard'
require_relative 'processor'
require_relative 'common'

processor = Processor.new

puts "#{GREEN_TEXT}RPN Calculator, ©2014, Phil Runninger #{CYAN_TEXT}#{'═' * (console_columns - 57)} Enter ? for help."
input = ''
begin
    begin
        answer = processor.execute(input)
    rescue Exception => msg
        answer = nil
        puts "#{RED_TEXT}#{msg}"
    end
    processor.registers.each{|name, value|
        print "#{BROWN_TEXT}#{name}#{GRAY_TEXT}=#{BROWN_TEXT}#{processor.format(value)} " 
    }
    print "#{BLUE_TEXT}• " if processor.registers != {}
    processor.stack.each{|value| print "#{GRAY_TEXT}#{processor.format(value)} " }
    print "#{GREEN_TEXT}#{processor.radix}► #{BROWN_TEXT}"
    input = Readline.readline('', true)
    Readline::HISTORY.pop if Readline::HISTORY.length>1 &&  Readline::HISTORY[-1] == Readline::HISTORY[-2]
end while input > ''

puts "#{CYAN_TEXT}#{'═' * (console_columns - 23)} Thanks for using rpn."
if !answer.nil?
    Clipboard.copy processor.format(answer).to_s
    puts "#{GRAY_TEXT}#{processor.format(answer)} #{BROWN_TEXT}was copied to the clipboard."
end
puts "#{RESET_COLORS} "
