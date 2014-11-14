-module(utils).


% create dummy data of specified size in bytes
create_data(Size) ->
	% conver to size from bytes to bits
	Bits = Size*8,

	% return the data
	<<0:Bits>>.


% create data of specified size in bytes, taking a list of integers as initial values
create_data(Size, Integers) ->
	% convert the integer list to binary
	IntegerBinary = list_to_binary(Integers),

	% calculate the remaining size to pad
	RemainingSize = Size - length(Integers),

	RemainingBinary = case RemainingSize > 0 of
		true ->
			% create padding data
			create_data(RemainingSize);

		% the size of the integer list is bigger than the input size ...
		false ->
			% ... so no padding
			<<"">>

	end,

	% return result
	<<IntegerBinary/binary, RemainingBinary/binary>>.


% create an event
create_event(Type, PrivateData, Composite) ->
	% get the primality of the event
	Primality = case Composite of
		true ->
			composite;

		false ->
			primitive

	end,

	% build and return the event
	[
		{type, Type},
		{primality, Primality},
		{private_data, PrivateData},
		{source, node()}
	].


create_event(Type, PrivateData) ->
create_event(Type, PrivateData, true).


create_event(Type) ->
	create_event(Type, <<"">>).
