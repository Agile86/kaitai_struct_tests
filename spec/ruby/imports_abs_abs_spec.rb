# Autogenerated from KST: please remove this line if doing any edits by hand!

RSpec.describe 'ImportsAbsAbs' do
  it 'parses test properly' do
    require 'imports_abs_abs'
    r = ImportsAbsAbs.from_file('src/fixed_struct.bin')

    expect(r.one).to eq 80
    expect(r.two.one).to eq 65
    expect(r.two.two.one).to eq 67
  end
end
