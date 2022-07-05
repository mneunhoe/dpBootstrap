# -*- coding: utf-8 -*-
"""census_dp_gan_refactored.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1-1gH6RcAP3QVVhPXL-8wpTJ0cBfLbDnJ

First we need to install and load some python libraries. The neural networks are implemented in torch and the opacus library is used for differential privcay.
"""
import argparse
import os
import numpy as np
import math
import sys

import torchvision.transforms as transforms
from torchvision.utils import save_image

from torch.utils.data import Dataset, DataLoader
from torchvision import datasets
from torch.autograd import Variable

from opacus import PrivacyEngine

import torch.nn as nn
import torch.nn.functional as F
import torch

"""The next code chunk sets up the classes for the GAN network used for synthesis."""

# The Generator Network will contain so called residual blocks. These pass the output and the input of a layer to the next layer
class ResidualBlock(nn.Module):
  def __init__(self, i, o):
    super(ResidualBlock, self).__init__()
    self.fc = nn.Linear(i, o)
    self.leaky_relu = nn.LeakyReLU()

  def forward(self, input):
    out = self.fc(input)
    out = self.leaky_relu(out)

    return torch.cat((out, input), dim = 1)


class Generator(nn.Module):
  def __init__(self, noise_dim = 5, data_dim = 5, hidden_units = (128, 128), dropout_rate = 0.5):
    super(Generator, self).__init__()
    self.seq = nn.Sequential()

    dim = noise_dim

    i = 1

    for neurons in hidden_units:
      self.seq.add_module(module = ResidualBlock(dim, neurons), name = "res_block_" + str(i))
      #self.seq.add_module(module = nn.ReLU(), name = "activation_" + str(i))
      self.seq.add_module(module = nn.Dropout(dropout_rate), name = "dropout_" + str(i))

      dim = dim + neurons
      i = i + 1
    
    self.seq.add_module(module = nn.Linear(dim, data_dim), name = "output")
    
  
  def forward(self, input):
    return self.seq(input)


class Discriminator(nn.Module):
  def __init__(self, data_dim = 5, hidden_units = (128, 128), dropout_rate = 0.5, prob = False):
    super(Discriminator, self).__init__()
    self.seq = nn.Sequential()

    dim = data_dim

    i = 1

    for neurons in hidden_units:
      self.seq.add_module(module = nn.Linear(dim, neurons), name = "linear_" + str(i))
      self.seq.add_module(module = nn.ReLU(), name = "activation_" + str(i))
      self.seq.add_module(module = nn.Dropout(dropout_rate), name = "dropout_" + str(i))

      dim = neurons
      i = i + 1
    
    self.seq.add_module(module = nn.Linear(dim, 1), name = "output")
    if prob:
      self.seq.add_module(module = nn.Sigmoid(), name = "sigmoid_output")

  def forward(self, input):
    return self.seq(input)


class WeightClipper(object):

    def __init__(self, frequency=5):
        self.frequency = frequency

    def __call__(self, module):
        # filter the variables to get the ones you want
        if hasattr(module, 'weight'):
            w = module.weight.data
            w = w.clamp(-0.1,0.1)

"""With the new version of opacus the dataset needs to come in as a class thats compatible with the torch dataloader class. Change the path to the csv accordingly."""

class CustomDataset(Dataset):
  def __init__(self, id = 1):
    self.full_data = np.genfromtxt("https://www.dropbox.com/s/63e8dazzac3gvbt/100_log_train_samples.csv?raw=1", delimiter = ",", skip_header = 0)
    self.data = self.full_data[self.full_data[:,10] == id,0:10]
  
  def __len__(self):
    return len(self.data)    
  
  def __getitem__(self, idx):
    data_tensor = torch.from_numpy(self.data[idx]).float().to("cpu")
    return data_tensor

"""Define one update step of the GAN model."""

def train_step(d_net, g_net, d_optim, g_optim, clipper, data, dataset, conditional=False):

  real_data = data
  
  batch_size = len(real_data)
  
  if conditional:
    z_sel = np.random.choice(len(dataset), batch_size)
    z_labels = torch.from_numpy(dataset.full_data[z_sel, 5:10]).float().to("cpu")
    z = torch.randn((batch_size, 5)).to("cpu")
    z = torch.cat((z,z_labels), dim = 1)
  else:
    z = torch.randn((batch_size, 10)).to("cpu")

  with torch.no_grad():
    fake_data = g_net(z)
    if conditional:
      fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
    else:
      gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
      fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
    

  

  d_optim.zero_grad()

  d_loss = -(torch.mean(d_net(real_data)) - torch.mean(d_net(fake_data))).mean()

  d_net.apply(clipper)
  d_loss.backward()

  d_optim.step()

  

  if conditional:
    z_sel = np.random.choice(len(dataset), batch_size)
    z_labels = torch.from_numpy(dataset.full_data[z_sel, 5:10]).float().to("cpu")
    z = torch.randn((batch_size, 5)).to("cpu")
    z = torch.cat((z,z_labels), dim = 1)
  else:
    z = torch.randn((batch_size, 10)).to("cpu")
  
  fake_data = g_net(z)
  if conditional:
    fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
  else:
    gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
    fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
  
  g_optim.zero_grad()
  
  d_optim.zero_grad()
  dis_fake = d_net(fake_data)
  

  g_loss = torch.mean(dis_fake)

  g_loss = -g_loss.mean()

  

  g_loss.backward()

  g_optim.step()

  # print(f"D Loss: {np.mean(d_loss.item()):.6f}\t"
  #       f"G_Loss: {np.mean(g_loss.item()):.6f}")

"""Run the experiments for each of the 10 samples and all values of epsilon.
First, with the conditional GAN synthesizer.
"""


def setup_dp_gan(epochs, epsilon, n, dataloader, conditional = False):
  
  if conditional:
    g_net = Generator(noise_dim = 10, data_dim = 5,  hidden_units=(128, 128, 128))
  else:
    g_net = Generator(noise_dim = 10, data_dim = 10,  hidden_units=(128, 128, 128))

  d_net = Discriminator(data_dim = 10, hidden_units=(128, 128, 128), prob = False, dropout_rate=0)

  g_optim = torch.optim.RMSprop(g_net.parameters(), lr = 0.0005)

  d_optim = torch.optim.RMSprop(d_net.parameters(), lr = 0.0005*4)
    
  privacy_engine = PrivacyEngine()

  d_net, d_optim, dataloader = privacy_engine.make_private_with_epsilon(
    module=d_net,
    optimizer=d_optim,
    data_loader=dataloader,
    epochs=epochs,
    target_epsilon=epsilon,
    target_delta=1/n**2,
    max_grad_norm=1.0
    )

  clipper = WeightClipper()
    
  return d_net, g_net, d_optim, g_optim, clipper, privacy_engine

def training_loop(epochs, dataloader, dataset, d_net, g_net, d_optim, g_optim, clipper, conditional=False):
    for epoch in range(epochs):
    # For each batch in the dataloader
      for i, data in enumerate(dataloader, 0):
        train_step(d_net, g_net, d_optim, g_optim, clipper, data, dataset, conditional=conditional)
    
#     eps = privacy_engine.get_epsilon(delta=1/n**2)
# 
#     print(eps)
# 
# 
#     def my_ceil(a, precision=0):
#         return np.true_divide(np.ceil(a * 10**precision), 10**precision)
# 
#     def my_floor(a, precision=0):
#         return np.true_divide(np.floor(a * 10**precision), 10**precision)
# 
#     my_ceil(eps, 1)
# 
#     if conditional:
#       z_labels = torch.from_numpy(dataset.full_data[:,5:10]).float().to("cpu")
#       z = torch.randn((n, 5)).to("cpu")
#       z = torch.cat((z,z_labels), dim = 1)
#     else:
#       z = torch.randn((n, 10)).to("cpu")
# 
# 
#     with torch.no_grad():
#       fake_data = g_net(z)
#       if conditional:
#         fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
#       else:
#         gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
#         fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
# 
# 
#     for i in range(1, 11):
#       if conditional:
#         z_labels = torch.from_numpy(dataset.full_data[:,5:10]).float().to("cpu")
#         z = torch.randn((6300, 5)).to("cpu")
#         z = torch.cat((z,z_labels), dim = 1)
#       else:
#         z = torch.randn((6300, 10)).to("cpu")
# 
# 
#       with torch.no_grad():
#         fake_data = g_net(z)
#         if conditional:
#           fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
#         else:
#           gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
#           fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
# 
#       a = fake_data.detach().cpu().numpy()
#       if np.ceil(eps) < 20:
#         if conditional:
#           np.savetxt("census_res" + str(sample) + "/cgan_dp_" + str(sample) + "_" + str(my_ceil(eps, 1)) + "_" + str(i) + ".csv", a, delimiter=",")
#         else:
#           np.savetxt("census_res" + str(sample) + "/gan_dp_" + str(sample) + "_" + str(my_ceil(eps, 1)) + "_" + str(i) + ".csv", a, delimiter=",")
#       else:
#         if conditional:
#           np.savetxt("census_res" + str(sample) + "/cgan_dp_" + str(sample) + "_" + "no" + "_" + str(i) + ".csv", a, delimiter=",")
#         else:
#           np.savetxt("census_res" + str(sample) + "/gan_dp_" + str(sample) + "_" + "no" + "_" + str(i) + ".csv", a, delimiter=",")
# 
# """Second, with the unconditional synthesizer."""
# 
# # How many examples do we sample per update step?
# batch_size = 64
# 
# 
# # Epochs: How many times (on average) do we want to pass the entire data set.
# epochs = 100
# 
# 
# # Over how many different original samples do we iterate? (In the paper 10.)
# samples = range(1, 11)
# 
# # What are the target epsilons? (1000 for no privacy, essentially no noise is added)
# epsilons =[0.5, 1, 5, 10, 1000]
# 
# # Do we want to use the conditional GAN (conditional on stratum membership) or the unconditional one=
# conditional=False
# 
# # Run the experiments for all samples and all values of epsilon (Will take some time. Approx 15 minutes per sample and epsilon)
# for sample in samples:
#   if not os.path.isdir("census_res" + str(sample)):
#     os.mkdir("census_res" + str(sample))
#   for epsilon in epsilons:
#     print(epsilon)
#     dataset = CustomDataset(id = sample)
# 
#     n = len(dataset)    
# 
#     dataloader = DataLoader(dataset, batch_size=batch_size)
# 
#     if conditional:
#       g_net = Generator(noise_dim = 10, data_dim = 5,  hidden_units=(128, 128, 128))
#     else:
#       g_net = Generator(noise_dim = 10, data_dim = 10,  hidden_units=(128, 128, 128))
# 
#     d_net = Discriminator(data_dim = 10, hidden_units=(128, 128, 128), prob = False, dropout_rate=0)
# 
#     g_optim = torch.optim.RMSprop(g_net.parameters(), lr = 0.0005)
# 
#     d_optim = torch.optim.RMSprop(d_net.parameters(), lr = 0.0005*4)
#     
#     privacy_engine = PrivacyEngine()
# 
#     d_net, d_optim, dataloader = privacy_engine.make_private_with_epsilon(
#     module=d_net,
#     optimizer=d_optim,
#     data_loader=dataloader,
#     epochs=epochs,
#     target_epsilon=epsilon,
#     target_delta=1/n**2,
#     max_grad_norm=1.0
#     )
# 
#     clipper = WeightClipper()
# 
# 
#     for epoch in range(epochs):
#     # For each batch in the dataloader
#       for i, data in enumerate(dataloader, 0):
#         train_step(d_net, g_net, d_optim, g_optim, data, dataset, conditional=conditional)
#     
#     eps = privacy_engine.get_epsilon(delta=1/n**2)
# 
#     print(eps)
# 
# 
#     def my_ceil(a, precision=0):
#         return np.true_divide(np.ceil(a * 10**precision), 10**precision)
# 
#     def my_floor(a, precision=0):
#         return np.true_divide(np.floor(a * 10**precision), 10**precision)
# 
#     my_ceil(eps, 1)
# 
#     if conditional:
#       z_labels = torch.from_numpy(dataset.full_data[:,5:10]).float().to("cpu")
#       z = torch.randn((n, 5)).to("cpu")
#       z = torch.cat((z,z_labels), dim = 1)
#     else:
#       z = torch.randn((n, 10)).to("cpu")
# 
# 
#     with torch.no_grad():
#       fake_data = g_net(z)
#       if conditional:
#         fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
#       else:
#         gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
#         fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
# 
# 
#     for i in range(1, 11):
#       if conditional:
#         z_labels = torch.from_numpy(dataset.full_data[:,5:10]).float().to("cpu")
#         z = torch.randn((6300, 5)).to("cpu")
#         z = torch.cat((z,z_labels), dim = 1)
#       else:
#         z = torch.randn((6300, 10)).to("cpu")
# 
# 
#       with torch.no_grad():
#         fake_data = g_net(z)
#         if conditional:
#           fake_data = torch.cat((fake_data[:,0:5], z_labels), dim = 1)
#         else:
#           gumbel_data = F.gumbel_softmax(fake_data[:,5:10], tau = 0.2)
#           fake_data = torch.cat((fake_data[:,0:5], gumbel_data), dim = 1)
# 
#       a = fake_data.detach().cpu().numpy()
#       if np.ceil(eps) < 20:
#         if conditional:
#           np.savetxt("census_res" + str(sample) + "/cgan_dp_" + str(sample) + "_" + str(my_ceil(eps, 1)) + "_" + str(i) + ".csv", a, delimiter=",")
#         else:
#           np.savetxt("census_res" + str(sample) + "/gan_dp_" + str(sample) + "_" + str(my_ceil(eps, 1)) + "_" + str(i) + ".csv", a, delimiter=",")
#       else:
#         if conditional:
#           np.savetxt("census_res" + str(sample) + "/cgan_dp_" + str(sample) + "_" + "no" + "_" + str(i) + ".csv", a, delimiter=",")
#         else:
#           np.savetxt("census_res" + str(sample) + "/gan_dp_" + str(sample) + "_" + "no" + "_" + str(i) + ".csv", a, delimiter=",")
# 
# """If running on a colab instance you need to zip the contents of the results folder to be able to download. If running on a local instance, the results will be directly in the root folder of the computer."""
# 
# !zip -r /content.zip /content
