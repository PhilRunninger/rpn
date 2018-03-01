$LOAD_PATH.unshift File.dirname($0)

require 'colorize'
require 'readline'
require 'clipboard'
require_relative 'processor'


if ARGV.length > 0
  processor = Processor.new nil
  answer = processor.execute(ARGV.join(' '))
  puts processor.stack.map{|value| "#{processor.format(value)}" }.join(' ').colorize(processor.settings.color_normal)
  Clipboard.copy processor.format(answer).to_s unless answer.nil?
else
  processor = Processor.new File.join(Dir.home, '.rpnrc')
  puts "RPN Calculator, ©2016, Phil Runninger ".colorize(processor.settings.color_title) + 
    "#{'═' * (processor.console_columns - 57)} Enter ? for help.".colorize(processor.settings.color_help)
  input = ''
  begin
    begin
      answer = processor.execute(input)
    rescue Exception => exception
      answer = nil
      puts exception.message.colorize(processor.settings.color_error)
    end

    prompt = ''

    prompt += "Recording #{processor.recording}... ".colorize(processor.settings.color_error) unless processor.recording.nil?
    prompt += "#{processor.macros.keys.join(', ')}  ".colorize(processor.settings.color_help) unless processor.macros == {}

    prompt += processor.registers.map{|name, value|
      name.colorize(processor.settings.color_register) +
        '='.colorize(processor.settings.color_normal) +
        processor.format(value).colorize(processor.settings.color_register)}.join(' ')
    prompt += ' • '.colorize(processor.settings.color_help_heading) unless processor.registers.empty?

    prompt += processor.stack.map{|value| "#{processor.format(value)}" }.join(' ').colorize(processor.settings.color_normal)
    prompt += ' ' unless processor.stack.empty?

    prompt += processor.radix.colorize(processor.settings.color_title) + ' ' unless processor.radix.empty?

    prompt += "#{processor.angle}► ".colorize(processor.settings.color_title)

    input = Readline.readline(prompt.strip, true)
    Readline::HISTORY.pop if Readline::HISTORY.length>1 &&  Readline::HISTORY[-1] == Readline::HISTORY[-2]
  end while input > ''

  puts "#{'═' * (processor.console_columns - 23)} Thanks for using rpn.".colorize(processor.settings.color_help)
  if !answer.nil?
    Clipboard.copy processor.format(answer).to_s
    puts processor.format(answer).colorize(processor.settings.color_normal) +
      ' was copied to the clipboard.'.colorize(processor.settings.color_register)
  end
end
