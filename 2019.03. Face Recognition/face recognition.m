%%
close all;
clear all;
% read all traning images
subject01 = 11;
subject14 = 10;
img_dims = [243 320]; % 77760
subject01path = 'yalefaces/trainingset01/';
subject14path = 'yalefaces/trainingset14/'
testpath = 'yalefaces/subject14.test.gif';
file = 's*';
subject01_filenames = dir([subject01path file]);
subject14_filenames = dir([subject14path file]);

% subject 01
A = [];
for i = 1 : subject01
    filename = [subject01path subject01_filenames(i).name];
    a = imread(filename);
    vec = reshape(a,243*320,1);
    A = [A vec];
end
A = double(A);
% subject 14
B = [];
for i = 1 : subject14
    filename = [subject14path subject14_filenames(i).name];
    a = imread(filename);
    vec = reshape(a,243*320,1);
    B = [B vec];
end
B = double(B);

%% find the mean face of subject 14
mean_face14 = mean(B,2);
face14 = reshape(uint8(mean_face14),243,320);
imshow(face14);

%% eigenfaces for subject 14
B1 = B;
for i = 1 : subject14
    B1(:,i) = B(:,i) - mean_face14;
end
B1 = double(B1);
% peform PCA on the data matrix
C2 = (1/subject14) * B1' * B1;
% calculate the top 6 eigenvectors and eigenvalues
[V2,D2] = eigs(C2,6);
% compute the eigenfaces
eigenface2 = [];
for i = 1 : 6
    mv = B1 * V2(:,i);
    mv = mv/norm(mv);
    eigenface2 = [eigenface2 mv];
end

% display the first 6 eigenfaces for subject 14
figure;
for i = 1:6
    im = eigenface2(:,i);
    im = reshape(im,243,320);
    subplot(2,3,i);
    im = imagesc(im);colormap('gray');
end

%% subject 01
mean_face01 = mean(A,2);
A1 = A;
for i = 1 : subject01
    A1(:,i) = A(:,i) - mean_face01;
end
A1 = double(A1);
% peform PCA on the data matrix
C1 = (1/subject01) * A1' * A1;
% calculate the first eigenvectors and eigenvalues
[V1,D1] = eigs(C1,1);
% compute the eigenfaces
eigenface1 = [];
mv = A1 * V1(:,1);
mv = mv/norm(mv);
eigenface1 = [eigenface1 mv];


%% project test image use only the first principle component
% read the test image
input_img = imread(testpath);
input_img = reshape(input_img,243*320,1);
mean_face = (mean_face01+mean_face14)/2
temp = double(input_img) - mean_face;
% project it using the first component
feature_vec1 = temp' * eigenface1;
feature_vec1 % 2.3500e+03
feature_vec2 = temp' * eigenface2(:,1);
feature_vec2 % 8.3069e+03

% test image belongs to subject 14
