function output = arithmetic_decode(stream, prob, target_size)

TOP_VALUE = hex2dec('7FFFFFFF');
FIRST_QTR = hex2dec('20000000');
HALF = hex2dec('40000000');
THIRD_QTR = hex2dec('60000000');

low = 0;
high = TOP_VALUE;
value = 0;
idx = 1;
bounds = [0, cumsum(prob)];
stream = [stream zeros(1, 50)];
output = zeros(1, target_size);

for i=1:31
    value = value * 2 + stream(idx);
    idx = idx + 1;
end
for i = 1:target_size
    step = high - low + 1;
    cum = ceil((value - low) / step * bounds(end));

    symbol = find(bounds >= cum, 1) - 2;
    output(i) = symbol;
    high = low + ceil(step / bounds(end) * bounds(symbol + 2)) - 1;
    low = low + floor(step / bounds(end) * bounds(symbol + 1));

    while true
        if high < HALF
            low = low * 2;
            high = high * 2 + 1;
            value = value * 2 + stream(idx);
            idx = idx + 1;
        elseif low >= HALF
            low = (low - HALF) * 2;
            high = ( high - HALF ) * 2 + 1;
            value = (value - HALF) * 2 + stream(idx);
            idx = idx + 1;
        elseif low >= FIRST_QTR && high < THIRD_QTR
            low = ( low - FIRST_QTR ) * 2;
            high = ( high - FIRST_QTR ) * 2 + 1;
            value = (value - FIRST_QTR) * 2 + stream(idx);
            idx = idx + 1;
        else
            break
        end
    end
end
end
