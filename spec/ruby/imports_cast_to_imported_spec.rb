# Autogenerated from KST: please remove this line if doing any edits by hand!

RSpec.describe 'ImportsCastToImported' do
  it 'parses test properly' do
    require 'imports_cast_to_imported'
    r = ImportsCastToImported.from_file('src/process_xor_4.bin')

    expect(r.hw.one).to eq 236
    expect(r.two.hw_one).to eq 236
  end
end
