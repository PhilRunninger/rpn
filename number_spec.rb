require_relative 'number'

describe Number do
    it 'initializes numbers of all supported types' do
        test_initialize([{:input=>'12.34',   :value=>12.34,   :base_as_entered=>0},
                         {:input=>'-12.34',  :value=>-12.34,  :base_as_entered=>0},
                         {:input=>'12e3',    :value=>12000,   :base_as_entered=>0},
                         {:input=>'-12e4',   :value=>-120000, :base_as_entered=>0},
                         {:input=>'0b1001',  :value=>9,       :base_as_entered=>2},
                         {:input=>'-0b1001', :value=>-9,      :base_as_entered=>2},
                         {:input=>'012',     :value=>10,      :base_as_entered=>8},
                         {:input=>'-012',    :value=>-10,     :base_as_entered=>8},
                         {:input=>'12',      :value=>12,      :base_as_entered=>10},
                         {:input=>'-12',     :value=>-12,     :base_as_entered=>10},
                         {:input=>'0x12',    :value=>18,      :base_as_entered=>16},
                         {:input=>'-0x12',   :value=>-18,     :base_as_entered=>16},
                         {:input=>'sin',     :value=>nil,     :base_as_entered=>nil}
        ])
    end

  def test_initialize(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new condition[:input]
      expect(x.value).to eq(condition[:value])
      expect(x.base_as_entered).to eq(condition[:base_as_entered])
    }
  end

  it 'formats numbers properly given current mode' do
      test_format([{:input=>"123",   :base=>0, :expected=>"123"},
                   {:input=>"9.7",   :base=>0, :expected=>"9.7"},
                   {:input=>"0xa",   :base=>0, :expected=>"0xa"},
                   {:input=>"015",   :base=>0, :expected=>"015"},
                   {:input=>"0b101", :base=>0, :expected=>"0b101"},
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
                   {:input=>"123",   :base=>8, :expected=>"0173"},
                   {:input=>"9.7",   :base=>8, :expected=>"012"},
                   {:input=>"0xa",   :base=>8, :expected=>"012"},
                   {:input=>"015",   :base=>8, :expected=>"015"},
                   {:input=>"0b101", :base=>8, :expected=>"05"},
                   {:input=>"123",   :base=>2, :expected=>"0b1111011"},
                   {:input=>"9.7",   :base=>2, :expected=>"0b1010"},
                   {:input=>"0xa",   :base=>2, :expected=>"0b1010"},
                   {:input=>"015",   :base=>2, :expected=>"0b1101"},
                   {:input=>"0b101", :base=>2, :expected=>"0b101"}
      ])
  end

  def test_format(conditions)
    conditions.each{|condition|
      print "~"
      x = Number.new condition[:input]
      expect(x.format(condition[:base])).to eq(condition[:expected])
    }
  end
end

# vim:ft=ruby foldmethod=marker sw=4
