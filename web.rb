require 'sinatra'
require 'json'
require './main'

get '/' do
  erb :index
end

post '/possible-matches' do
  begin
    p = Peptide.new params[:peptide_sequence]
    possible_sequences = p.possible_sequences params[:weight].to_f
    { 'weight' => p.weight, 'possible_sequences' => possible_sequences }.to_json
  rescue BadSequenceError => e
    { 'possible_sequences' => e.message }.to_json
  end
end
