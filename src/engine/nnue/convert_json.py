#!/usr/bin/env python
# Simple script converting json nets to the binary representation used by carp
import sys
import json
import struct

FEATURES = 768
HIDDEN = 384
QA = 255
QB = 64
QAB = QA * QB

def write_bytes(array, path):
    with open(path, 'wb') as file:
        for num in array:
            file.write(struct.pack('<h', num))

def convert_weight(json_weight, stride, length, q, transpose):
    weights = [0 for _ in range(length)]

    for i, row in enumerate(json_weight):
        for j, weight in enumerate(row):
            if transpose:
                index = j * stride + i
            else:
                index = i * stride + j
            
            weights[index] = int(weight * q)

    return weights

def convert_bias(json_bias, q):
    biases = []

    for bias in json_bias:
        value = int(bias * q)
        biases.append(value)
    
    return biases

# Check for correct number of command line arguments
if len(sys.argv) != 2:
    print("Usage: python convert_json.py <json_file>")
    sys.exit(1)

json_file = sys.argv[1]
with open(json_file, 'r') as file:
    data = json.load(file)

for key, value in data.items():
    if key == "ft.weight":
        weights = convert_weight(value, HIDDEN, HIDDEN * FEATURES, QA, True)
        write_bytes(weights, "net/feature_weights.bin")
    elif key == "ft.bias":
        biases = convert_bias(value, QA)
        write_bytes(biases, "net/feature_bias.bin")
    elif key == "out.weight":
        weights = convert_weight(value, HIDDEN * 2, HIDDEN * 2, QB, False)
        write_bytes(weights, "net/output_weights.bin")
    elif key == "out.bias":
        biases = convert_bias(value, QAB)    
        write_bytes(biases, "net/output_bias.bin")
