def foo from_units, to_units
  all_units = UNITS_CONVERSION.map{|x| x['systems'].map{|y| y['conversions'].map{|z| z['unit']}}}.flatten
  raise ArgumentError, "Bad units: #{from_units}. Type 'units' to see valid units." unless all_units.include?(from_units)
  raise ArgumentError, "Bad units: #{to_units}. Type 'units' to see valid units." unless all_units.include?(to_units)

  f = UNITS_CONVERSION.find{|y| y['systems'].find{|x| x['conversions'].find{|z| z['unit']==from_units}}}
  t = UNITS_CONVERSION.find{|y| y['systems'].find{|x| x['conversions'].find{|z| z['unit']==to_units}}}
  #puts f
  #puts t

  raise ArgumentError, "Incompatible units, Type 'units' to see valid units." unless f['category'] == t['category']

  f_standard = f['systems'].find{|x| x['conversions'].find{|y| y['unit']==from_units}}['standard']
  t_standard = t['systems'].find{|x| x['conversions'].find{|y| y['unit']==to_units}}['standard']
  #puts f_standard
  #puts t_standard

  from = f['systems'].find{|x| x['standard']==f_standard}['conversions'].find{|z| z['unit']==from_units}['from']
  #puts from

  translation = f_standard == t_standard ? '' : f['translations'].find{|x| x['from']==f_standard && x['to']==t_standard}['translation']
  #puts translation

  to = f['systems'].find{|x| x['standard']==t_standard}['conversions'].find{|z| z['unit']==to_units}['to']
  #puts to
  puts "execue \"#{from} #{translation} #{to}\""
end

UNITS_CONVERSION = [{'category' => 'length',
                     'systems' => [{'standard' => 'm',
                                    'conversions' => [{'unit' => 'nm',     'from' => '1e9 /',                     'to' => '1e9 *'},
                                                      {'unit' => 'micron', 'from' => '1e6 /',                     'to' => '1e6 *'},
                                                      {'unit' => 'mm',     'from' => '1000 /',                    'to' => '1000 *'},
                                                      {'unit' => 'cm',     'from' => '100 /',                     'to' => '100 *'},
                                                      {'unit' => 'm',      'from' => '',                          'to' => ''},
                                                      {'unit' => 'km',     'from' => '1000 *',                    'to' => '1000 /'},
                                                      {'unit' => 'ly',     'from' => '9.4607304725808e15 *',      'to' => '9.4607304725808e15 /'}]},
                                   {'standard' => 'in',
                                    'conversions' => [ {'unit' => 'in',     'from' => '',             'to' => ''},
                                                       {'unit' => 'ft',     'from' => '12 *',        'to' => '12 /'},
                                                       {'unit' => 'yd',     'from' => '3 * 12 *',    'to' => '12 / 3 /'},
                                                       {'unit' => 'mi',     'from' => '5280 * 12 *', 'to' => '12 / 5280 /'}]}],
                     'translations' => [{'from'=>'m', 'to'=>'in', 'translation'=>'100 * 2.54 /'},
                                        {'from'=>'in', 'to'=>'m', 'translation'=>'2.54 * 100 /'}]},
                    {'category' => 'weight (force)',
                     'systems'=>[{'standard' => 'kg',
                                  'conversions' => [{'unit' => 'mg', 'from' => '1000000 /', 'to' => '1000000 *'},
                                                    {'unit' => 'g', 'from' => '1000 /', 'to' => '1000 *'},
                                                    {'unit' => 'kg', 'from' => '', 'to' => ''},
                                                    {'unit' => 'lb', 'from' => '0.45359 *', 'to' => '0.45359 /'},
                                                    {'unit' => 'ton', 'from' => '2000 0.45359 * *', 'to' => '0.45359 2000 * /'},
                                                    {'unit' => 'N', 'from' => '9.80665002864 /', 'to' => '9.80665002864 *'}]}]},
                    {'category' => 'pressure',
                     'systems' => [{'standard' => 'pa',
                                    'conversions' => [{'unit' => 'psi', 'from' => '6894.75728001037 *' , 'to' => '6894.75728001037 /'},
                                                      {'unit' => 'kg/cm2', 'from' => '0.000010197162129779282 /', 'to' => '0.000010197162129779282 *'}]}]},
                    {'category' => 'kitchen volumes',
                     'systems' => [{'standard' => 'tsp',
                                    'conversions' => [{'unit' => 'tsp', 'from' => '', 'to' => ''},
                                                      {'unit' => 'tbsp', 'from' => '3 *', 'to' => '3 /'},
                                                      {'unit' => 'ounce', 'from' => '3 2 * *', 'to' => '3 2 * /'},
                                                      {'unit' => 'cup', 'from' => '16 * 3 *', 'to' => '3 / 16 /'},
                                                      {'unit' => 'pint', 'from' => '2 16 3 * * *', 'to' => '2 16 3 * * /'},
                                                      {'unit' => 'quart', 'from' => '2 2 16 3 * * * *', 'to' => '2 2 16 3 * * * /'},
                                                      {'unit' => 'gallon', 'from' => '4 2 2 16 3 * * * * *', 'to' => '4 2 2 16 3 * * * * /'}]}]},
                    {'category' => 'temperature',
                     'systems' => [{'standard' => 'C',
                                    'conversions' => [{'unit' => 'C', 'from' => '', 'to' => ''},
                                                      {'unit' => 'F', 'from' => '32 - 5 * 9 /', 'to' => '9 * 5 / 32 +'},
                                                      {'unit' => 'K', 'from' => '273.15 -', 'to' => '273.15 +'}]}]},
                    {'category' => 'angle',
                     'systems' => [{'standard' => 'rad',
                                    'conversions' => [{'unit' => 'rad', 'from' => '', 'to' => ''},
                                                      {'unit' => 'deg', 'from' => 'pi * 180 /', 'to' => '180 * pi /'}]}]}
                   ]

from_units = 'ly'
to_units = 'yd'

foo from_units, to_units

