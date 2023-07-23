# Autogenerated from KST: please remove this line if doing any edits by hand!

RSpec.describe 'ExprToITrailing' do
  it 'parses test properly' do
    require 'expr_to_i_trailing'
    r = ExprToITrailing.from_file('src/term_strz.bin')

    expect {
      r.to_i_r10
    }.to raise_error(ArgumentError)
    expect(r.to_i_r13).to eq 44020937
    expect {
      r.to_i_garbage
    }.to raise_error(ArgumentError)
  end
end
