# Twitter Clone Project

## Overview
The Twitter clone project is an implementation designed to simulate the core functionalities of Twitter using Erlang, with a frontend built in React. The system includes modules for both client and server operations, handling user interactions such as tweeting, following, retweeting, and querying hashtags and mentions. The project also utilizes webhooks to enable real-time updates and notifications.

## Features
- **Client Module**: Manages user actions and simulates interactions.
- **Server Module**: Coordinates user actions, maintains user states, and distributes tweets among followers.
- **React Frontend**: Provides an interactive and user-friendly interface.
- **Webhooks**: Enable real-time updates and notifications.

## Project Structure
### Backend (Erlang)
- **t_client.erl**: Manages user initialization, assigns followers, and simulates user interactions.
- **t_server.erl**: Coordinates user actions, maintains user states, and distributes tweets.

### Frontend (React)
- **components/**: Contains React components for UI elements.
- **pages/**: Contains main pages for different routes.
- **services/**: Manages API calls to the backend.

### Configuration Files
- **Common.cfg**: Defines common configuration parameters for the network.
- **PeerInfo.cfg**: Holds information about peers within the network.

## Detailed Description
### Backend (Erlang)
#### t_client.erl
- **initUser/3**: Initializes a user and assigns followers.
- **assignSubscribers/3**: Assigns a random number of followers to a user.
- **beginSim/3**: Simulates user interactions like tweeting, following, querying hashtags, and mentions.
- **makeTweet/3**: Generates a tweet and sends a message to post it.

#### t_server.erl
- **start/2**: Starts the server and initializes the simulation.
- **simulator/4**: Main simulation function that handles incoming messages and performs operations based on received messages.
- **postTweet/2**: Posts a tweet for a user and updates subscribers' feeds.
- **switchOnline/2**, **switchOffline/2**: Handles switching users online or offline.

### Frontend (React)
#### Components
- **Tweet.js**: Displays individual tweets.
- **UserFeed.js**: Displays the user's feed.
- **FollowButton.js**: Button to follow/unfollow users.

#### Pages
- **HomePage.js**: Main page displaying the user's feed and tweet input.
- **ProfilePage.js**: Displays user profile information and their tweets.

#### Services
- **api.js**: Manages API calls to the Erlang backend for actions like tweeting, following, and querying.

## Real-Time Updates
Webhooks are used to enable real-time updates and notifications. Whenever a user tweets, follows, or performs other actions, webhooks notify the relevant components to update the UI instantly.

## Setup and Running the Project
### Backend (Erlang)
1. **Compile the Erlang files**:
    ```bash
    erlc t_client.erl
    erlc t_server.erl
    ```
2. **Start the Erlang shell**:
    ```bash
    erl
    ```
3. **Run the server**:
    ```erlang
    t_server:start(NumUsers, NumTweets).
    ```

### Frontend (React)
1. **Install dependencies**:
    ```bash
    npm install
    ```
2. **Start the development server**:
    ```bash
    npm start
    ```
