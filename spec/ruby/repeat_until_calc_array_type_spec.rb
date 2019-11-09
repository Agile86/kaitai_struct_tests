# https://github.com/kaitai-io/kaitai_struct/issues/561

require 'repeat_until_calc_array_type'

RSpec.describe RepeatUntilCalcArrayType do
  it 'parses test properly' do
    r = RepeatUntilCalcArrayType.from_file('src/repeat_until_process.bin')

    expect(r.records.length).to eq 3
    expect(r.records[0].marker).to eq 232
    expect(r.records[0].body).to eq 2863311546
    expect(r.records[1].marker).to eq 250
    expect(r.records[1].body).to eq 2863315102
    expect(r.records[2].marker).to eq 170
    expect(r.records[2].body).to eq 1431655765
  end
end
