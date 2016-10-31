require 'json'
require_relative 'number'

describe Number do
    it 'initializes numbers of all supported types' do
        test_initialize([{:input=>'0',       :value=>0,       :base_as_entered=>0},
                         {:input=>'12.34',   :value=>12.34,   :base_as_entered=>0},
                         {:input=>'-12.34',  :value=>-12.34,  :base_as_entered=>0},
                         {:input=>'12e3',    :value=>12000,   :base_as_entered=>0},
                         {:input=>'-12e4',   :value=>-120000, :base_as_entered=>0},
                         {:input=>'0b1001',  :value=>9,       :base_as_entered=>2},
                         {:input=>'-0b1001', :value=>-9,      :base_as_entered=>2},
                         {:input=>'012',     :value=>10,      :base_as_entered=>8},
                         {:input=>'-012',    :value=>-10,     :base_as_entered=>8},
                         {:input=>'12',      :value=>12,      :base_as_entered=>0},
                         {:input=>'-12',     :value=>-12,     :base_as_entered=>0},
                         {:input=>'0x12',    :value=>18,      :base_as_entered=>16},
                         {:input=>'-0x12',   :value=>-18,     :base_as_entered=>16},
                         {:input=>42,        :value=>42,      :base_as_entered=>0},
                         {:input=>4.2,       :value=>4.2,     :base_as_entered=>0}
        ])
    end

  def test_initialize(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new(condition[:input])
      expect(x.value).to eq(condition[:value])
      expect(x.value).to be_kind_of(Float)
      expect(x.base_as_entered).to eq(condition[:base_as_entered])
    }
  end

  it 'throws an exception for invalid numbers' do
      expect{Number.new('0129')}.to raise_error
      expect{Number.new('sin')}.to raise_error
  end

  it 'formats numbers properly given current mode' do
      test_format([{:input=>"123",   :base=>0,  :expected=>"123"},
                   {:input=>"9.7",   :base=>0,  :expected=>"9.7"},
                   {:input=>"0xa",   :base=>0,  :expected=>"0xa"},
                   {:input=>"015",   :base=>0,  :expected=>"015"},
                   {:input=>"0b101", :base=>0,  :expected=>"0b101"},
                   {:input=>"123",   :base=>10, :expected=>"123"},
                   {:input=>"9.7",   :base=>10, :expected=>"10"},
                   {:input=>"0xa",   :base=>10, :expected=>"10"},
                   {:input=>"015",   :base=>10, :expected=>"13"},
                   {:input=>"0b101", :base=>10, :expected=>"5"},
                   {:input=>"123",   :base=>16, :expected=>"0x7b"},
                   {:input=>"9.7",   :base=>16, :expected=>"0xa"},
                   {:input=>"0xa",   :base=>16, :expected=>"0xa"},
                   {:input=>"015",   :base=>16, :expected=>"0xd"},
                   {:input=>"0b101", :base=>16, :expected=>"0x5"},
                   {:input=>"123",   :base=>8,  :expected=>"0173"},
                   {:input=>"9.7",   :base=>8,  :expected=>"012"},
                   {:input=>"0xa",   :base=>8,  :expected=>"012"},
                   {:input=>"015",   :base=>8,  :expected=>"015"},
                   {:input=>"0b101", :base=>8,  :expected=>"05"},
                   {:input=>"123",   :base=>2,  :expected=>"0b1111011"},
                   {:input=>"9.7",   :base=>2,  :expected=>"0b1010"},
                   {:input=>"0xa",   :base=>2,  :expected=>"0b1010"},
                   {:input=>"015",   :base=>2,  :expected=>"0b1101"},
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
    expect(x.to_h).to eq({"value"=>31.0, "base_as_entered"=>16})
  end

  it 'unhashes into a Number object' do
    x = Number.from_h({"value"=>30.0, "base_as_entered"=>8})
    expect(x.value).to eq(30)
    expect(x.base_as_entered).to eq(8)
  end

end

# vim:ft=ruby foldmethod=marker sw=4
