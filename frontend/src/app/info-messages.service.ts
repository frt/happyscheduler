import { Injectable } from '@angular/core';

@Injectable()
export class InfoMessagesService {
    public infoMessage: string;

    constructor() {
        this.infoMessage = '';
    }

}