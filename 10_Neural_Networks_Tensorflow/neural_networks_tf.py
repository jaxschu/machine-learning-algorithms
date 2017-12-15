
############################################################################
## Implement a two layer neural network in Tensorflow to classify MNIST digits ##
############################################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Train a two layer neural network to classify the MNIST dataset ##
## Use Relu as the activation function for the first layer. Use Softmax as the activation function for the second layer##
## z=Relu(x*W1+b1) ##
## y=Softmax(z*W2+b2)##
# Use cross-entropy as the loss function#
# Tip: be careful when you initialize the weight and bias parameters.
## You only need to install the CPU version of Tensorflow on your laptop, which is much easier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import sys
import os
os.environ["CUDA_VISIBLE_DEVICES"]=""

from tensorflow.examples.tutorials.mnist import input_data
import tensorflow as tf

FLAGS = None


def main(_):
  # Import data
  mnist = input_data.read_data_sets(FLAGS.data_dir, one_hot=True)

  # Create the model
  #######################
  ## FILL IN CODE HERE ##
  #######################
  num_iterations = 10000
  num_hidden = 500
  learning_rate = 0.1

  input_pics = tf.placeholder(tf.float32, [None,784])
  output_vals = tf.placeholder(tf.float32, [None, 10])

  l1_weights = tf.Variable(tf.random_uniform([784, num_hidden], -0.001, 0.001)) #we use random_uniform to improve acc
  l1_biases = tf.Variable(tf.zeros([num_hidden]))

  l2_weights = tf.Variable(tf.random_uniform([num_hidden, 10], -0.001, 0.001)) #second layer
  l2_biases = tf.Variable(tf.zeros([10]))

  layer1 = tf.matmul(input_pics, l1_weights)
  layer2 = tf.nn.relu(layer1 + l1_biases)
  digit_weights = tf.matmul(layer2, l2_weights) + l2_biases
    
  loss_function = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=digit_weights, labels=output_vals))
  optimizer = tf.train.GradientDescentOptimizer(0.5).minimize(loss_function)
  
  correct_pred = tf.equal(tf.argmax(digit_weights,1), tf.argmax(output_vals,1))
  accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32))

  tf.global_variables_initializer().run()


  for i in range (num_iterations):
    batch = mnist.train.next_batch(100)
    optimizer.run(feed_dict={input_pics: batch[0], output_vals: batch[1]})
    if (i % 100 == 0):
        print("Training iteration: " + str(i) + " Testing Accuracy: " + str(accuracy.eval(feed_dict={input_pics: mnist.test.images, output_vals: mnist.test.labels})))
        
    

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--data_dir', type=str, default='/tmp/tensorflow/mnist/input_data',
                      help='Directory for storing input data')
  FLAGS, unparsed = parser.parse_known_args()
  tf.app.run(main=main, argv=[sys.argv[0]] + unparsed)





