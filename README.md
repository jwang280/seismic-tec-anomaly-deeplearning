# Ionospheric TEC anomaly as an earthquake precursor
This project is to discover the relationship between Ionospheric Total Free-Electron Content(TEC) anomaly and earthquakes in New Zealand by deploying deep learning algorithms.

The amount of TEC values varies with the amount of radiation received from the Sun.Thus, TEC values have seasonal variations, TEC values drop since there is less radiation received in winter, and increases in summer.It also has a strong daily fluctuation, TEC values are high during daytime and near-zero at night-time.Free-electrons imposes a dispersive delay on the GPS satellite ranging signals broadcasting.The affection on the ranging signals also varies with TEC values.

In addition, during earthquake preparations, free-electrons are released from the ground and change the local ionospheric TEC values in a relatively short period of time.When is the TEC anomaly occurs due to earthquake preparations remain controversial? Between 3 days and 2 weeks before and after the earthquakes are discussed in different research articles.

This project started from collect the original GPS raw data.There are two types of data, one is the GPS observation data from local GeoNet, powered by GNS Science, the other is the navigation data, which is the ephemeris data for GPS constellation, from Scripps Orbit and Permanent Array Center in California. 
Both types of raw data are in receiver independent exchange(RINEX) format, which enables users to post-process the GPS stations received data and generates more accurate results.

GPS Toolkit(GPSTk), an open-source computing suite to the satellite navigation community produced by Space and Geophysics Laboratory within the Applied Research Laboratories of the University of Texas at Austin, to process GPS associated satellite navigation system data (https://gitlab.com/sgl-ut/GPSTk), is utilised to translate the raw GPS data.

There are three main programs from the GPSTk are used in this project.
1. ResCor, computes any residuals and correlations, and then register extended RINEX observation data.
2. Ionobias, continue reads the extended RINEX observation data to estimate satellite and receiver biases and to compute a simple ionospheric model using least squares and the slant TEC values.
3. TECMaps, reads the output data from both ResCor and IonoBias programs, to create maps of the TEC.

The assumption of this series of process is, the ionosphere is a thin shell about 350 kilometres height above the ground, by knowing this fixed vertical TEC height, the oblique angle of the GPS ranging signal, and the Ionospheric Pierce Point(IPP), where the ranging signal goes through the thin shell, the slant length from the TEC to the receiver can be computed. Put both of the computed factors and estimated factors that without TEC affections, into a least-square model, the TEC value at the corresponding IPP can be estimated. Finally, a TEC map is plotted throughout all gridded IPPs.

After the daily 2880 TEC maps are generated(with 30 seconds interval), according to the Fortran code, another assumption is adopted, which is if the gridded TEC values are standing out in a relatively large area and the anomaly time is relatively long, this TEC anomaly is more likely caused by solar activities or other geomagnetic effects, otherwise, the anomaly has a better chance caused by pre-earthquake TEC anomaly(PETA).

In terms of this assumption, daily PETA correlation files are generated. Based on these PETA correlation values, data visualization PETA images are generated, to show all the gridded correlation values that cover the geographical distribution of New Zealand.

These geographical NumPy array data formed images, can be systematically analysed by deep learning methods, to analyse the underlying true relationship between the TEC anomaly and earthquakes. ConvLSTM2D neural network method from TensorFlow, which considers both spatial and temporal matters at the same time, is deployed in this project.

The input data is the 50 by 50 by 1 shaped NumPy array images, there are also some data preprocessing methods that are necessary before the model is trained.
1. 29 major earthquake events from 2003, which excludes aftershocks that are within 3 days, and 24 days of data, 21 days before the event and 2 days after, are selected for the input data. There are 29 events in total.
2. Add the normalised sunspot numbers that scaled from 0 to 1, to be the second layer of the data, which takes TEC affected by Solar radiations received into account.
3. Add the normalised day of the year that scaled from -1 to 1, to be the third layer, which takes how many of those sunspot numbers are actually affecting the local TEC as another coefficient.
4. Data splitting of 26 events in the training and 3 events in the test is applied.
5. A time sequence length of 10 days is applied, thus 15 sequences in each event.
6. Data labelling for the 15 sequences of each event are, 0.4 for the event day, 0.25 the one day apart, and 0.05 for the two days apart, otherwise 0.
7. Resampling strategy is used to solve the 'class' imbalance.
8. Training data is shuffled before the training, to maximize the distance between neighbour samples.
9. Bayesian Optimizer is utilized for hyperparameter tuning in some parts of the training.
