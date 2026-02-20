export class CommitGossip {
  constructor({ onMessage } = {}) {
    this.onMessage = onMessage || (() => {});
    this.channel = null;
  }

  attachDataChannel(dataChannel) {
    this.channel = dataChannel;
    this.channel.onmessage = (event) => {
      try {
        const payload = JSON.parse(String(event.data));
        this.onMessage(payload);
      } catch {
        // Ignore malformed gossip payloads.
      }
    };
  }

  broadcast(payload) {
    if (!this.channel || this.channel.readyState !== "open") return false;
    this.channel.send(JSON.stringify(payload));
    return true;
  }
}
