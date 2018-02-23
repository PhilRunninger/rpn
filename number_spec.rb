require 'json'
require_relative 'number'

describe Number do
    it 'initializes numbers of all supported types' do
        test_initialize([{:input=>'0',           :value=>0,        :type=>"Float",   :base_as_entered=>0},
                         {:input=>'12.34',       :value=>12.34,    :type=>"Float",   :base_as_entered=>0},
                         {:input=>'-12.34',      :value=>-12.34,   :type=>"Float",   :base_as_entered=>0},
                         {:input=>'12e3',        :value=>12000,    :type=>"Float",   :base_as_entered=>0},
                         {:input=>'1.2E3',       :value=>1200,     :type=>"Float",   :base_as_entered=>0},
                         {:input=>'-12e4',       :value=>-120000,  :type=>"Float",   :base_as_entered=>0},
                         {:input=>'0b1001',      :value=>9,        :type=>"Float",   :base_as_entered=>2},
                         {:input=>'-0b1001',     :value=>-9,       :type=>"Float",   :base_as_entered=>2},
                         {:input=>'0o12',        :value=>10,       :type=>"Float",   :base_as_entered=>8},
                         {:input=>'-0o12',       :value=>-10,      :type=>"Float",   :base_as_entered=>8},
                         {:input=>'12',          :value=>12,       :type=>"Float",   :base_as_entered=>0},
                         {:input=>'-12',         :value=>-12,      :type=>"Float",   :base_as_entered=>0},
                         {:input=>'0x12',        :value=>18,       :type=>"Float",   :base_as_entered=>16},
                         {:input=>'0x1a',        :value=>26,       :type=>"Float",   :base_as_entered=>16},
                         {:input=>'-0x12',       :value=>-18,      :type=>"Float",   :base_as_entered=>16},
                         {:input=>'-0x2c',       :value=>-44,      :type=>"Float",   :base_as_entered=>16},
                         {:input=>42,            :value=>42,       :type=>"Float",   :base_as_entered=>0},
                         {:input=>4.2,           :value=>4.2,      :type=>"Float",   :base_as_entered=>0},
                         {:input=>'1+2i',        :value=>1+2i,     :type=>"Complex", :base_as_entered=>-1},
                         {:input=>'0b101+0o72i', :value=>5+58i,    :type=>"Complex", :base_as_entered=>-1},
                         {:input=>'0x2A+1.2E3i', :value=>42+1200i, :type=>"Complex", :base_as_entered=>-1},
                         {:input=>'0x2A+0b111i', :value=>42+7i,    :type=>"Complex", :base_as_entered=>-1}
        ])
    end

  def test_initialize(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new(condition[:input])
      expect(x.value).to eq(condition[:value])
      expect(x.value.class.to_s).to eq(condition[:type])
      expect(x.base_as_entered).to eq(condition[:base_as_entered])
    }
  end

  it 'throws an exception for invalid numbers' do
      expect{Number.new('0129')}.to raise_error(ArgumentError)
      expect{Number.new('sin')}.to raise_error(ArgumentError)
  end

  it 'formats numbers properly given current mode' do
      test_format([{:input=>"123",   :base=>0,  :expected=>"123"},
                   {:input=>"9.7",   :base=>0,  :expected=>"9.7"},
                   {:input=>"0xa",   :base=>0,  :expected=>"0xa"},
                   {:input=>"0o15",  :base=>0,  :expected=>"0o15"},
                   {:input=>"0b101", :base=>0,  :expected=>"0b101"},
                   {:input=>"123",   :base=>10, :expected=>"123"},
                   {:input=>"9.7",   :base=>10, :expected=>"10"},
                   {:input=>"0xa",   :base=>10, :expected=>"10"},
                   {:input=>"0o15",  :base=>10, :expected=>"13"},
                   {:input=>"0b101", :base=>10, :expected=>"5"},
                   {:input=>"123",   :base=>16, :expected=>"0x7b"},
                   {:input=>"9.7",   :base=>16, :expected=>"0xa"},
                   {:input=>"0xa",   :base=>16, :expected=>"0xa"},
                   {:input=>"0o15",  :base=>16, :expected=>"0xd"},
                   {:input=>"0b101", :base=>16, :expected=>"0x5"},
                   {:input=>"123",   :base=>8,  :expected=>"0o173"},
                   {:input=>"9.7",   :base=>8,  :expected=>"0o12"},
                   {:input=>"0xa",   :base=>8,  :expected=>"0o12"},
                   {:input=>"0o15",  :base=>8,  :expected=>"0o15"},
                   {:input=>"0b101", :base=>8,  :expected=>"0o5"},
                   {:input=>"123",   :base=>2,  :expected=>"0b1111011"},
                   {:input=>"9.7",   :base=>2,  :expected=>"0b1010"},
                   {:input=>"0xa",   :base=>2,  :expected=>"0b1010"},
                   {:input=>"0o15",  :base=>2,  :expected=>"0b1101"},
                   {:input=>"0b101", :base=>2,  :expected=>"0b101"}
      ])
  end

  def test_format(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new(condition[:input])
      expect(x.format(condition[:base])).to eq(condition[:expected])
    }
  end

  it 'supports equality comparisons' do
      x = Number.new("123")
      expect(x).to eq(Number.new("123"))
  end

  it 'hashes a Number object)' do
    x = Number.new('0x1f')
    expect(x.to_h).to eq({"16"=>31.0})
  end

  it 'unhashes into a Number object' do
    x = Number.from_h({"8"=>30.0})
    expect(x.value).to eq(30)
    expect(x.base_as_entered).to eq(8)
  end

end

# vim:ft=ruby foldmethod=marker sw=4
