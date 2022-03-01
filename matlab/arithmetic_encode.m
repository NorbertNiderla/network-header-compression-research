function output = arithmetic_encode(input, prob)

% TODO: input check

TOP_VALUE = hex2dec('7FFFFFFF');
FIRST_QTR = hex2dec('20000000');
HALF = hex2dec('40000000');
THIRD_QTR = hex2dec('60000000');
high = TOP_VALUE;
low = 0;
output = zeros(1, length(input) * 8);
output_idx = 1;
bounds = [0, cumsum(prob)];
scale = 0;

for symbol = input
    step = high - low + 1;
    high = low + ceil(step / bounds(end) * bounds(symbol + 2)) - 1;
    low = low + floor(step / bounds(end) * bounds(symbol + 1));

    while true
        if high < HALF
            output(output_idx) = 0;
            output_idx = output_idx + 1;
            low = low * 2;
            high = high * 2 + 1;
            while scale > 0
                output(output_idx) = 1;
                output_idx = output_idx + 1;
                scale = scale - 1;
            end
        elseif low >= HALF
            output(output_idx) = 1;
            output_idx = output_idx + 1;
            low = ( low - HALF ) * 2;
            high = (high - HALF) * 2 + 1;
            while scale > 0
                output(output_idx) = 0;
                output_idx = output_idx + 1;
                scale = scale - 1;
            end
        elseif low >= FIRST_QTR && high < THIRD_QTR
            scale = scale + 1;
            low = (low - FIRST_QTR) * 2;
            high = (high - FIRST_QTR) * 2 + 1;
        else
            break
        end
    end
end

if low < FIRST_QTR
    output(output_idx) = 0;
    output_idx = output_idx + 1;
    for i = 0:scale
        output(output_idx) = 1;
        output_idx = output_idx + 1;
    end
else
    output(output_idx) = 1;
    output_idx = output_idx + 1;
end

output = output(1:output_idx - 1);
end

