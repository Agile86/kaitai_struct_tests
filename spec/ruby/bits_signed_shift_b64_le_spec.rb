# Autogenerated from KST: please remove this line if doing any edits by hand!

load_err = nil
begin
  require 'bits_signed_shift_b64_le'
rescue SyntaxError => e
  load_err = e
  BitsSignedShiftB64Le = nil
rescue LoadError => e
  load_err = e
  BitsSignedShiftB64Le = nil
end

RSpec.describe BitsSignedShiftB64Le do
  it 'parses test properly' do
    raise load_err if BitsSignedShiftB64Le.nil?
    r = BitsSignedShiftB64Le.from_file('src/bits_signed_shift_b64_le.bin')

    expect(r.a).to eq 0
    expect(r.b).to eq 255
  end
end