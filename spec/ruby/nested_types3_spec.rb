# Autogenerated from KST: please remove this line if doing any edits by hand!

require 'nested_types3'

RSpec.describe NestedTypes3 do
  it 'parses test properly' do
    r = NestedTypes3.from_file('src/fixed_struct.bin')

    expect(r.a_cc.value_cc).to eq 80
    expect(r.a_c_d.value_d).to eq 65
    expect(r.b.value_b).to eq 67
    expect(r.b.a_cc.value_cc).to eq 75
    expect(r.b.a_c_d.value_d).to eq 45
  end
end