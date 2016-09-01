require 'nested_types2'

RSpec.describe NestedTypes2 do
  it 'parses test properly' do
    r = NestedTypes2.from_file('src/fixed_struct.bin')

    expect(r.one.typed_at_root.value_b).to eq 80

    expect(r.one.typed_here1.value_c).to eq 65

    expect(r.one.typed_here1.typed_here.value_d).to eq 67
    expect(r.one.typed_here1.typed_parent.value_cc).to eq 75
    expect(r.one.typed_here1.typed_root.value_b).to eq 45

    expect(r.one.typed_here2.value_cc).to eq 49

    expect(r.two.value_b).to eq -1
  end
end
