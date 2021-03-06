require 'json'
require_relative 'number'

describe Number do
  it 'initializes numbers of all supported types' do
    test_initialize([{:input=>'0',           :value=>0,               :type=>"Float",   :base_as_entered=>0},
                     {:input=>'12.34',       :value=>12.34,           :type=>"Float",   :base_as_entered=>0},
                     {:input=>'-12.34',      :value=>-12.34,          :type=>"Float",   :base_as_entered=>0},
                     {:input=>'12e3',        :value=>12000,           :type=>"Float",   :base_as_entered=>0},
                     {:input=>'1.2E3',       :value=>1200,            :type=>"Float",   :base_as_entered=>0},
                     {:input=>'-12e4',       :value=>-120000,         :type=>"Float",   :base_as_entered=>0},
                     {:input=>'0b1001',      :value=>9,               :type=>"Float",   :base_as_entered=>2},
                     {:input=>'-0b1001',     :value=>-9,              :type=>"Float",   :base_as_entered=>2},
                     {:input=>'0o12',        :value=>10,              :type=>"Float",   :base_as_entered=>8},
                     {:input=>'-0o12',       :value=>-10,             :type=>"Float",   :base_as_entered=>8},
                     {:input=>'12',          :value=>12,              :type=>"Float",   :base_as_entered=>0},
                     {:input=>'-12',         :value=>-12,             :type=>"Float",   :base_as_entered=>0},
                     {:input=>'0x12',        :value=>18,              :type=>"Float",   :base_as_entered=>16},
                     {:input=>'0x1a',        :value=>26,              :type=>"Float",   :base_as_entered=>16},
                     {:input=>'-0x12',       :value=>-18,             :type=>"Float",   :base_as_entered=>16},
                     {:input=>'-0x2c',       :value=>-44,             :type=>"Float",   :base_as_entered=>16},
                     {:input=>42,            :value=>42,              :type=>"Float",   :base_as_entered=>0},
                     {:input=>4.2,           :value=>4.2,             :type=>"Float",   :base_as_entered=>0},
                     {:input=>'1+2i',        :value=>1+2i,            :type=>"Complex", :base_as_entered=>0},
                     {:input=>'0b101+0o72i', :value=>5+58i,           :type=>"Complex", :base_as_entered=>0},
                     {:input=>'0x2A+1.2E3i', :value=>42+1200i,        :type=>"Complex", :base_as_entered=>0},
                     {:input=>'0x2A+0b111i', :value=>42+7i,           :type=>"Complex", :base_as_entered=>0},
                     {:input=>1+2i,          :value=>1+2i,            :type=>"Complex", :base_as_entered=>0},
                     {:input=>'Infinity',    :value=>Float::INFINITY, :type=>"Float",   :base_as_entered=>0}
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
    test_format([{:input=>"Infinity", :base=>0,  :expected=>"Infinity"},
                 {:input=>"123",      :base=>0,  :expected=>"123"},
                 {:input=>"9.7",      :base=>0,  :expected=>"9.7"},
                 {:input=>"0xa",      :base=>0,  :expected=>"0xa"},
                 {:input=>"0o15",     :base=>0,  :expected=>"0o15"},
                 {:input=>"0b101",    :base=>0,  :expected=>"0b101"},
                 {:input=>"2+3i",     :base=>0,  :expected=>"2+3i"},
                 {:input=>"123",      :base=>10, :expected=>"123"},
                 {:input=>"9.7",      :base=>10, :expected=>"10"},
                 {:input=>"0xa",      :base=>10, :expected=>"10"},
                 {:input=>"0o15",     :base=>10, :expected=>"13"},
                 {:input=>"0b101",    :base=>10, :expected=>"5"},
                 {:input=>"2+3i",     :base=>10, :expected=>"2+3i"},
                 {:input=>"123",      :base=>16, :expected=>"0x7b"},
                 {:input=>"9.7",      :base=>16, :expected=>"0xa"},
                 {:input=>"0xa",      :base=>16, :expected=>"0xa"},
                 {:input=>"0o15",     :base=>16, :expected=>"0xd"},
                 {:input=>"0b101",    :base=>16, :expected=>"0x5"},
                 {:input=>"2+3i",     :base=>16, :expected=>"0x2+0x3i"},
                 {:input=>"123",      :base=>8,  :expected=>"0o173"},
                 {:input=>"9.7",      :base=>8,  :expected=>"0o12"},
                 {:input=>"0xa",      :base=>8,  :expected=>"0o12"},
                 {:input=>"0o15",     :base=>8,  :expected=>"0o15"},
                 {:input=>"0b101",    :base=>8,  :expected=>"0o5"},
                 {:input=>"2+3i",     :base=>8,  :expected=>"0o2+0o3i"},
                 {:input=>"123",      :base=>2,  :expected=>"0b1111011"},
                 {:input=>"9.7",      :base=>2,  :expected=>"0b1010"},
                 {:input=>"0xa",      :base=>2,  :expected=>"0b1010"},
                 {:input=>"0o15",     :base=>2,  :expected=>"0b1101"},
                 {:input=>"0b101",    :base=>2,  :expected=>"0b101"},
                 {:input=>"2+3i",     :base=>2,  :expected=>"0b10+0b11i"}
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
    test_to_h([{:input=>'Infinity', :expected=>{"0"=>"Infinity"}},
               {:input=>'0x1f', :expected=>{"16"=>31.0}},
               {:input=>'3+2i', :expected=>{"0"=>"3+2i"}},
               {:input=>'0o3+0x12i', :expected=>{"0"=>"3+18i"}}
    ])
  end

  def test_to_h(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new(condition[:input])
      expect(x.to_h).to eq(condition[:expected])
    }
  end

  it 'unhashes into a Number object' do
    test_from_h([{:input=>{"0"=>"Infinity"}, :expected=>Number.new("Infinity")},
                 {:input=>{"8"=>30.0}, :expected=>Number.new("0o36")},
                 {:input=>{"0"=>12}, :expected=>Number.new("12")},
                 {:input=>{"0"=>"3+7i"}, :expected=>Number.new("3+7i")}
    ])
  end

  def test_from_h(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.from_h(condition[:input])
      expect(x).to eq(condition[:expected])
    }
  end

end
