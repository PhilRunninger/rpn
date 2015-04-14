$LOAD_PATH.unshift File.dirname($0)

require 'clipboard'
require_relative 'processor'
require_relative 'common'

def format(value)
    if value.kind_of?(Array)
        return "[#{value.map{|v| format(v)}.join(' ')}]"
    elsif value % 1 == 0
        value.round.to_s
    else
        value.to_s
    end
end

processor = Processor.new

puts "#{GREEN_TEXT}RPN Calculator, ©2014, Phil Runninger #{CYAN_TEXT}#{'═' * (console_columns - 57)} Enter ? for help."
print "#{GREEN_TEXT}► #{BROWN_TEXT}"
input = gets.chomp
while input > ''
    begin
        answer = processor.execute(input)
    rescue Exception => msg
        answer = nil
        puts "#{RED_TEXT}#{msg}"
    end
    processor.registers.each{|name, value|
        print "#{BROWN_TEXT}#{name}#{GRAY_TEXT}=#{BROWN_TEXT}#{format(value)} " 
    }
    print "#{BLUE_TEXT}• " if processor.registers != {}
    processor.stack.each{|value| print "#{GRAY_TEXT}#{format(value)} " }
    print "#{GREEN_TEXT}► #{BROWN_TEXT}"
    input = gets.chomp
end

puts "#{CYAN_TEXT}#{'═' * (console_columns - 23)} Thanks for using rpn."
if !answer.nil?
    Clipboard.copy format(answer).to_s
    puts "#{GRAY_TEXT}#{format(answer)} #{BROWN_TEXT}was copied to the clipboard."
end
puts "#{RESET_COLORS} "
