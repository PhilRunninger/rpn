$LOAD_PATH.unshift File.dirname($0)

require 'colorize'
require 'readline'
require 'clipboard'
require_relative 'processor'
require_relative 'common'

processor = Processor.new

puts "RPN Calculator, ©2014, Phil Runninger ".colorize(processor.settings.color_title) + 
  "#{'═' * (console_columns - 57)} Enter ? for help.".colorize(processor.settings.color_help)
input = ''
begin
  begin
    answer = processor.execute(input)
  rescue Exception => exception
    answer = nil
    puts exception.message.colorize(processor.settings.color_error)
  end
  processor.registers.each{|name, value|
    print name.colorize(processor.settings.color_register) +
      '='.colorize(processor.settings.color_normal) +
      processor.format(value).colorize(processor.settings.color_register) +
      ' '
  }
  print '• '.colorize(processor.settings.color_help_heading) if processor.registers != {}
  prompt  = processor.stack.map{|value| "#{processor.format(value)}" }.join(' ').colorize(processor.settings.color_normal)
  prompt += ' ' + processor.radix.colorize(processor.settings.color_title) unless processor.radix == ''
  prompt += " #{processor.angle_mode}► ".colorize(processor.settings.color_title)
  input = Readline.readline(prompt.strip, true)
  Readline::HISTORY.pop if Readline::HISTORY.length>1 &&  Readline::HISTORY[-1] == Readline::HISTORY[-2]
end while input > ''

puts "#{'═' * (console_columns - 23)} Thanks for using rpn.".colorize(processor.settings.color_help)
if !answer.nil?
  Clipboard.copy processor.format(answer).to_s
  puts processor.format(answer).colorize(processor.settings.color_normal) +
    ' was copied to the clipboard.'.colorize(processor.settings.color_register)
end
