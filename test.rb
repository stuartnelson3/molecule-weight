require 'rspec'
require './main'

describe "molecules" do
  it "splits on the user input" do
    expect(parse_user_input("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parse_user_input("A3L3TXZ")).to eq(%w(a 3l 3t x z))
  end

end

describe AminoAcid do
  it "calculates the molecule weight" do
    parsed_input = parse_user_input("AEF3AYXZ")
    expect(AminoAcid.new(parsed_input).weight).to be_within(0.1).of(818.39)
  end
end

