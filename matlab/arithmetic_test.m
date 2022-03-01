test(8, 10000, 20);
test(4, 10000, 4);
test(1, 10000, 0.5);

function test(bitwidth, len, dev)
data = round(abs(normrnd(0, dev, 1, len)));
data = data(data < 2^bitwidth);
prob = zeros(1, 2^bitwidth);

for i = 1:length(data)
    prob(data(i) + 1) = prob(data(i) + 1) + 1;
end

stream = arithmetic_encode(data, prob);
decoded_data = arithmetic_decode(stream, prob, length(data));

if data == decoded_data
    fprintf("arithmetic PASSED: %2d %2.1f %10d %0.2f\n", bitwidth, dev, ...
        length(data), length(stream)/length(data)/bitwidth);
else
    fprintf("FAILED");
end
end