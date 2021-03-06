require 'sinatra'
require 'json'
require './main'

get '/' do
  erb :index
end

post '/possible-matches' do
  begin
    if params[:peptide_sequence] == ""
      return { 'possible_sequences' => 'missing peptide sequence' }.to_json
    end
    if params[:weight] == "NaN"
      return { 'possible_sequences' => 'missing weight' }.to_json
    end

    mass_type = MassTypes.parse params[:mass_type]
    # TODO: Reject if no tolerance value given
    tolerance = params[:tolerance].to_f
    if tolerance < 5
      { 'possible_sequences' => 'error: tolerance below 5 Da' }.to_json
    end
    residues = AminoAcids.new(mass_type).residues
    p = Peptide.new params[:peptide_sequence], residues, params[:wildcards] || {}
    possible_sequences = p.possible_sequences(params[:weight].to_f, tolerance)
    { 'weight' => p.weight, 'possible_sequences' => possible_sequences }.to_json
  rescue BadSequenceError => e
    { 'possible_sequences' => e.message }.to_json
  rescue => e
    { 'possible_sequences' => e.message }.to_json
  end
end
